extern crate core;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TS;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::Ident;
use syn::{Expr, FnArg, ItemFn, PathArguments, ReturnType, Type};

const DEFAULT_ENV: &str = "__env__";
const DEFAULT_ARGS: &str = "__args__";
const ENV_TYPE: &str = "&LEnv";
const ARGS_TYPE: &str = "&im::Vector<LValue>";
const LVALUE_TYPE: &str = "LValue";
const REF_LVALUE_TYPE: &str = "&LValue";
const RESULT: &str = "__result__";
//const DEFAULT_RETURN : &str = "Ok(sompas_struct::lvalue::LValue::Nil)";
const DEFAULT_RETURN: &str = "Ok(String::new())";
const ERROR_TYPE: &str = "LRuntimeError";
const RETURN_TYPE: &str = "LResult";
const RESULT_TYPE: &str = "Result";
const EXTENDED_RETURN_TYPE: &str = "Result<LValue, LRuntimeError>";

#[proc_macro_attribute]
pub fn __macro(_: TokenStream, input: TokenStream) -> TokenStream {
    let fun: ItemFn = syn::parse_macro_input!(input);
    let result: Ident = syn::parse_str(RESULT).unwrap();
    let vis = fun.vis;
    let name = fun.sig.ident;
    let (env, args, params) = build_params(fun.sig.inputs);
    let output = match &fun.sig.output {
        ReturnType::Default => quote!(),
        ReturnType::Type(_, b) => quote!(: #b),
    };
    let expr_result = build_return(&result, &fun.sig.output);
    let body = fun.block.as_ref();
    let expanded = quote! {
         #vis fn #name(#env : &LEnv, #args: &im::Vector<LValue>) -> LResult
        {
            #params
            let #result #output = {||{
                #body
            }}();
            #expr_result
        }
    };
    //println!("expanded : {}", expanded);
    TokenStream::from(expanded)
}

#[inline]
fn build_params(params: Punctuated<FnArg, Comma>) -> (Ident, Ident, TS) {
    let mut env_redefined = false;
    let mut new_params = TS::new();

    let mut env: Ident = syn::parse_str(DEFAULT_ENV).unwrap();
    let mut args = syn::parse_str(DEFAULT_ARGS).unwrap();
    let type_env: Type = syn::parse_str::<Type>(ENV_TYPE).unwrap();
    let type_args: Type = syn::parse_str::<Type>(ARGS_TYPE).unwrap();
    let type_lvalue: Type = syn::parse_str::<Type>(LVALUE_TYPE).unwrap();
    let type_ref_lvalue: Type = syn::parse_str::<Type>(REF_LVALUE_TYPE).unwrap();

    let mut j;
    let len = params.len();
    for (i, param) in params.into_iter().enumerate() {
        j = i;
        if env_redefined {
            j -= 1;
        }
        if let FnArg::Typed(p) = param {
            let t = p.ty.as_ref();
            let var = p.pat.as_ref();
            //case where env ident is redefined
            if i == 0 && t == &type_env {
                //println!("env is redefined!");
                env = syn::parse_quote!(#var);
                env_redefined = true;
            } else if ((i == 0 && !env_redefined) || i == 1) && (i == len - 1) && t == &type_args {
                //println!("args is redefiend");
                args = syn::parse_quote!(#var);
            } else {
                let line = if t == &type_lvalue {
                    quote! {let #var : #t= #args[#j].clone();}
                } else if t == &type_ref_lvalue {
                    quote! {let #var : #t= &#args[#j];}
                } else {
                    quote! {let #var : #t= <#t>::try_from(&#args[#j]);}
                };
                //println!("t: {:?}", p.ty);

                new_params = quote! {#new_params
                    #line
                }
            }
        }
        //println!("param {}: {:?}", i, param)
    }

    (env, args, new_params)
}

#[inline]
fn build_return(ident: &Ident, expr: &ReturnType) -> TS {
    let default: Expr = syn::parse_str(DEFAULT_RETURN).unwrap();
    let ok: Type = syn::parse_str(LVALUE_TYPE).unwrap();
    let err: Type = syn::parse_str(ERROR_TYPE).unwrap();
    let r_type: Ident = syn::parse_str(RESULT_TYPE).unwrap();
    let normal_return_type: Vec<Type> = vec![
        syn::parse_str(RETURN_TYPE).unwrap(),
        syn::parse_str(EXTENDED_RETURN_TYPE).unwrap(),
    ];
    let classic_return = quote!(Ok(#ok::from(#ident)));
    return match expr {
        ReturnType::Default => return quote!(#default),
        ReturnType::Type(_, b) => {
            if normal_return_type.contains(b.as_ref()) {
                quote!(#ident)
            } else {
                if let Type::Path(t) = b.as_ref() {
                    if t.path.segments.len() == 1 {
                        let segment = &t.path.segments[0];
                        if &segment.ident == &r_type {
                            //println!("it is a result");
                            if let PathArguments::AngleBracketed(args) = &segment.arguments {
                                assert!(args.args.len() == 2);
                                let o = &args.args[0];
                                let e = &args.args[1];
                                let o: Type =
                                    syn::parse_str(quote!(#o).to_string().as_str()).unwrap();
                                let e: Type =
                                    syn::parse_str(quote!(#e).to_string().as_str()).unwrap();
                                if o != ok && e != err {
                                    quote! {
                                        match #ident {
                                            Ok(o) => Ok(#ok::from(o)),
                                            Err(e) => Err(#err::from(e))
                                        }
                                    }
                                } else if o == ok {
                                    //println!("result returns a LValue");
                                    quote!(#ident.map_err(|e| #err::from(e)))
                                } else if e == err {
                                    //println!("result returns a LRuntimeError");
                                    quote!(#ident.map(|o| #ok::from(o)))
                                } else {
                                    quote!(#ident)
                                }
                            } else {
                                panic!("should have been brackets")
                            }
                        } else {
                            classic_return
                        }
                    } else {
                        classic_return
                    }
                } else {
                    classic_return
                }
            }
        }
    };
}
