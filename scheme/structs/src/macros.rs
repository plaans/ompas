#[macro_export]
macro_rules! dyn_async {(
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    async
    fn $fname:ident<$lt:lifetime> ( $($args:tt)* ) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
    $( #[$attr] )*
    #[allow(unused_parens)]
    $pub
    fn $fname<$lt> ( $($args)* ) -> ::std::pin::Pin<::std::boxed::Box<
        dyn ::std::future::Future<Output = ($($Ret)?)>
            + ::std::marker::Send + $lt
    >>
    {
        ::std::boxed::Box::pin(async move { $($body)* })
    }
)}

#[macro_export]
macro_rules! symbol {
    ($x:expr) => {
        LValue::Symbol(std::sync::Arc::new($x))
    };
}

#[macro_export]
macro_rules! err {
    ($x:expr) => {
        LValue::Err(std::sync::Arc::new($x))
    };
}

#[macro_export]
macro_rules! string {
    ($x:expr) => {
        LValue::String(std::sync::Arc::new($x))
    };
}

#[macro_export]
macro_rules! list {
    ($($x:expr)*) => (
        LValue::List(vector!($($x)*))
    );
}

#[macro_export]
macro_rules! lfn {
    /*(async $vi:vis $fname:ident<$lt:lifetime>($arg:ident, $env:ident){$($body:tt)*}) => {
        #[macro_rules_attribute(dyn_async!)]
        async $vi fn $fname<$lt>($arg: &im::Vector<LValue>, $env: &LEnv) -> LResult {
            $($body)*
        }
    };*/
    ($vi:vis $fname:ident($arg:ident, $env:ident){$($body:tt)*}) => {
        $vi fn $fname($arg: &im::Vector<$crate::lvalue::LValue>, $env: &$crate::lenv::LEnv) -> $crate::lerror::LResult {
            $($body)*
        }
    };
    ($vi:vis $fname:ident{$($body:tt)*}) => {
        lfn! {$vi $fname(_,_){$($body)*}}
    };
    ($vi:vis $fname:ident(_, $env:ident){$($body:tt)*}) => {
        lfn! {$vi $fname(__args__,$env){$($body)*}}
        /*$vi fn $fname(_: &im::Vector<LValue>, $env: &LEnv) -> LResult {
            $($body)*
        }*/
    };
    ($vi:vis $fname:ident($arg:ident, _){$($body:tt)*}) => {
         lfn! {$vi $fname($arg,__env__){$($body)*}}

        /*$vi fn $fname(arg: &im::Vector<LValue>, _: &LEnv) -> LResult {
             $($body)*
        }*/
    };
    ($vi:vis $fname:ident(_, _){$($body:tt)*}) => {
        lfn! {$vi $fname(__arg__,__env__){$($body)*}}
        /*$vi fn $fname(_: &im::Vector<LValue>, _: &LEnv) -> LResult {
             $($body)*
        }*/
    };
}

/*lfn! {async pub test_6<'a>(arg, env) {
    Ok(LValue::Nil)
}}*/

/*
lfn! {test_1(_,_){Ok(LValue::Nil)}}
lfn! {pub test_2(arg,env)
{
let lvalue = LValue::Nil;
Ok(lvalue)}}
lfn! {test_3(arg,_){Ok(LValue::Nil)}}
lfn! {test_4(_,env){Ok(LValue::Nil)}}
lfn! {test_5{Ok(LValue::Nil)}}*/
