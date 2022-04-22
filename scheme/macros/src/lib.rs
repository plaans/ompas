extern crate core;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TS;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr, FnArg, ItemFn, Lifetime, PathArguments, ReturnType, Type};
use syn::{GenericArgument, Ident};

const DEFAULT_NO_ENV: &str = "__env__";
const DEFAULT_NO_ARGS: &str = "__args__";
const DEFAULT_ARGS: &str = "args";
const ENV_TYPE: &str = "LEnv";
const ARGS_TYPE: &str = "Vec<LValue>";
const REF_ENV_TYPE: &str = "&LEnv";
const REF_ARGS_TYPE: &str = "&[LValue]";
const LVALUE_TYPE: &str = "LValue";
const REF_LVALUE_TYPE: &str = "&LValue";
const RESULT: &str = "__result__";
const DEFAULT_RETURN: &str = "Ok(sompas_structs::lvalue::LValue::Nil)";
const ERROR_TYPE: &str = "LRuntimeError";
const RETURN_TYPE: &str = "LResult";
const RESULT_TYPE: &str = "Result";
const EXTENDED_RETURN_TYPE: &str = "Result<LValue, LRuntimeError>";
const DEFAULT_LIFETIME: &str = "'a";

#[proc_macro_attribute]
pub fn scheme_fn(_: TokenStream, input: TokenStream) -> TokenStream {
    let fun: ItemFn = syn::parse_macro_input!(input);
    let result: Ident = syn::parse_str(RESULT).unwrap();
    let vis = fun.vis;
    let name = fun.sig.ident;
    let (env, args, params) = build_params(fun.sig.inputs, None, &name);
    let output = match &fun.sig.output {
        ReturnType::Default => quote!(),
        ReturnType::Type(_, b) => quote!(: #b),
    };
    let expr_result = build_return(&result, &fun.sig.output);
    let body = fun.block.as_ref();
    let expanded = quote! {
         #vis fn #name(#env : &sompas_structs::lenv::LEnv, #args: &[sompas_structs::lvalue::LValue]) -> sompas_structs::lerror::LResult
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

#[proc_macro_attribute]
pub fn async_scheme_fn(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut defined_lt = false;

    let fun: ItemFn = syn::parse_macro_input!(input);
    if fun.sig.asyncness.is_none() {
        panic!("function should be async")
    }
    //println!("fun params :{:?}", fun.sig.generics.params);
    let params = &fun.sig.generics.params;
    let lt: Lifetime = if params.len() == 1 {
        let lt = &params[0];
        defined_lt = true;
        syn::parse_str(quote!(#lt).to_string().as_str())
            .unwrap_or_else(|e| panic!("expected a litefime: {}", e))
    } else if params.len() == 0 {
        syn::parse_str(DEFAULT_LIFETIME).unwrap()
    } else {
        panic!("expected at most a lifetime");
    };

    //println!("{}", lt);
    //return quote!(#fun).into()

    let result: Ident = syn::parse_str(RESULT).unwrap();
    let vis = fun.vis;
    let name = fun.sig.ident;
    let (env, args, params) = build_params(
        fun.sig.inputs,
        match defined_lt {
            true => Some(&lt),
            false => None,
        },
        &name,
    );
    let output = match &fun.sig.output {
        ReturnType::Default => quote!(),
        ReturnType::Type(_, b) => quote!(: #b),
    };
    let expr_result = build_return(&result, &fun.sig.output);
    let body = fun.block.as_ref();
    let expanded = quote! {
     #vis fn #name<#lt>(#env : & #lt sompas_structs::lenv::LEnv, #args: & #lt [sompas_structs::lvalue::LValue]) -> ::std::pin::Pin<::std::boxed::Box<
        dyn ::std::future::Future<Output = sompas_structs::lerror::LResult>
            + ::std::marker::Send + #lt
        >>

            {
        ::std::boxed::Box::pin(async move {
                #params
                let #result #output = {|| async move{
                    #body
                }}().await;
                #expr_result
            })
            }
    };
    //println!("expanded : {}", expanded);
    TokenStream::from(expanded)
}

const VECTOR_TYPE: &str = "Vec";

#[inline]
fn try_into_vectored_type(t: &Type) -> Option<Type> {
    let vector_type: Vec<Ident> = vec![syn::parse_str(VECTOR_TYPE).unwrap()];
    let lvalue_type: Type = syn::parse_str(LVALUE_TYPE).unwrap();
    if let Type::Path(t) = t {
        //println!("{}", quote!(#t));
        let last = t.path.segments.last().unwrap();
        //println!("{}", quote!(#last));
        if vector_type.contains(&last.ident) {
            //println!("it is a vector");
            if let PathArguments::AngleBracketed(args) = &last.arguments {
                assert!(args.args.len() == 1);
                let t = &args.args[0];
                if let GenericArgument::Type(t) = t {
                    if t != &lvalue_type {
                        return Some(t.clone());
                    }
                }
            }
        }
    }
    None
}

#[inline]
fn build_params(
    params: Punctuated<FnArg, Comma>,
    defined_lt: Option<&Lifetime>,
    fname: &Ident,
) -> (Ident, Ident, TS) {
    let fname = fname.to_string();
    let mut env_redefined = false;
    let mut args_redefined = false;
    let mut new_params = TS::new();
    let mut env: Ident = syn::parse_str(DEFAULT_NO_ENV).unwrap();
    let mut args: Ident = syn::parse_str(DEFAULT_ARGS).unwrap();
    if params.is_empty() {
        return (env, args, new_params);
    }
    let type_lvalue: Type = syn::parse_str::<Type>(format!("{}", LVALUE_TYPE).as_str()).unwrap();
    let type_ref_lvalue: Type = syn::parse_str::<Type>(REF_LVALUE_TYPE).unwrap();
    let (type_env, type_args) = if let Some(lt) = defined_lt {
        //println!("&{} {}", lt, ENV_TYPE);
        let type_env: Type =
            syn::parse_str::<Type>(format!("&{} {}", lt, ENV_TYPE).as_str()).unwrap();
        let type_args: Type =
            syn::parse_str::<Type>(format!("&{} {}", lt, ARGS_TYPE).as_str()).unwrap();
        (type_env, type_args)
    } else {
        let type_env: Type = syn::parse_str::<Type>(REF_ENV_TYPE).unwrap();
        let type_args: Type = syn::parse_str::<Type>(REF_ARGS_TYPE).unwrap();

        (type_env, type_args)
    };

    let mut first = true;

    let len = params.len();
    for (i, param) in params.into_iter().enumerate() {
        let mut j = i;
        let mut expected = len;
        if env_redefined {
            j -= 1;
            expected -= 1;
        }
        if let FnArg::Typed(p) = param {
            let t = p.ty.as_ref();
            let var = p.pat.as_ref();
            //println!("{:?}", t);
            //case where env ident is redefined
            if i == 0 && t == &type_env {
                //println!("env is redefined!");
                env = syn::parse_quote!(#var);
                env_redefined = true;
            }
            //using directly args en redefining its ident
            else if ((i == 0 && !env_redefined) || i == 1) && (i == len - 1) && t == &type_args {
                //println!("args is redefiend");
                args_redefined = true;
                args = syn::parse_quote!(#var);
                assert_eq!(len, i + 1);
                break;
            }
            //using a vector of a certain type
            else if let Some(t) = try_into_vectored_type(t) {
                new_params = quote! {
                    let #var: Vec<#t> = #args.iter()
                        .map(|arg| std::convert::TryFrom::try_from(arg))
                        .collect::<Result<Vec<#t>,_>>()
                        .map_err(|e| e.chain(#fname))?;
                };
                assert_eq!(len, i + 1);
                break;
            } else {
                if first {
                    new_params = quote! {
                        if #args.len() != #expected {
                            return Err(sompas_structs::lerror::LRuntimeError::wrong_number_of_args(
                                #fname,
                                #args,
                                #expected..#expected,
                            ));
                        }
                    };
                    first = false
                }
                let line = if t == &type_lvalue {
                    quote! {let #var : #t= #args[#j].clone();}
                } else if t == &type_ref_lvalue {
                    quote! {let #var : #t= &#args[#j];}
                } else {
                    quote! {let #var : #t= <#t>::try_from(&#args[#j]).map_err(|e| e.chain(#fname))?;}
                };
                //println!("t: {:?}", t);

                new_params = quote! {#new_params
                    #line
                }
            }
        }
        //println!("param {}: {:?}", i, param)
    }

    if new_params.is_empty() && !args_redefined {
        args = syn::parse_str(DEFAULT_NO_ARGS).unwrap()
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
                                            Ok(o) => Ok(sompas_structs::lvalue::LValue::from(o)),
                                            Err(e) => Err(sompas_structs::lerror::LRuntimeError::from(e))
                                        }
                                    }
                                } else if o == ok {
                                    //println!("result returns a LValue");
                                    quote!(#ident.map_err(|e| sompas_structs::lerror::LRuntimeError::from(e)))
                                } else if e == err {
                                    //println!("result returns a LRuntimeError");
                                    quote!(#ident.map(|o| sompas_structs::lvalue::LValue::from(o)))
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
