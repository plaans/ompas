use ::macro_rules_attribute::macro_rules_attribute;

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
    ($($x:expr),*) => (
        LValue::List(vector!($($x),*))
    );
}

#[macro_export]
macro_rules! lfn {
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
    };
    ($vi:vis $fname:ident($arg:ident, _){$($body:tt)*}) => {
         lfn! {$vi $fname($arg,__env__){$($body)*}}
    };
    ($vi:vis $fname:ident(_, _){$($body:tt)*}) => {
        lfn! {$vi $fname(__arg__,__env__){$($body)*}}
    };
}

/*
Template of a function
pub fn <name>(p1: usize, p2: usize) -> anyhow::Result<O> {
    <body>
} =>
pub fn <name>(args: &im::Vector<LValue>, env: &LEnv) -> <0> {
    if length != 2 {
        Err()
    }
    let p1 = usize::try_from(args[0]
}
*/
#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::count!($($xs)*));
}

#[macro_export]
macro_rules! transform_arg {
    () => {};
    ($id:ident, $count:literal, $arg:tt, $t:ty) => {
        let $arg: $t = <$t>::try_from(&$id[$count])?;
    };
}
#[macro_export]
macro_rules! check_args {
    ($count:literal $id:ident) => {};
    ($count:literal $id:ident $arg1:tt $t:tt $($args:tt)*) => {
        let args;
        $crate::transform_arg!($id,$count,$arg1,$t);
        //let $arg1: $t1 = <$t1>::try_from(&args[0])?;
        $crate::check_args!($count $id $($args)*);
    };
}

#[macro_export]
macro_rules! lfn_extended {
(
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($env:ident : &LEnv $(,$($arg:tt : $t:tt),*)*) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
    $( #[$attr] )*
    #[allow(unused_parens)]
    #[function_name::named]
    $pub
    fn $fname($env: &LEnv, args : &im::Vector<LValue>) -> $crate::lerror::LResult
    {
        $crate::check_number_of_args!(args, $crate::count!($($($arg)*)*));
        $crate::check_args!(0 args $($($arg $t)*)*);
        let result = {
            $($body)*
        };
        result.into()
    }
);
    (
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($($arg:tt : $t:tt),* ) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
    $( #[$attr] )*
    #[allow(unused_parens)]
    #[function_name::named]
    $pub
    fn $fname(__env__: &$crate::lenv::LEnv, args : &im::Vector<$crate::lvalue::LValue>) -> $crate::lerror::LResult
    {

        $crate::check_number_of_args!(args, $crate::count!($($arg)*));
        $crate::check_args!(0 args $($arg $t)*);
        let result = {
            $($body)*
        };
        result.into()
    }
);
}

#[macro_export]
macro_rules! check_type {
    ($fname:expr, $lv:expr,$expected:expr) => {
        if $lv.get_kind() != $expected {
            return Err($crate::LRuntimeError::wrong_type(fname, lv, expected));
        }
    };
    ($lv:expr,$expected:expr) => {
        check_type!(function_name!(), $lv, $expected)
    };
}

#[macro_export]
macro_rules! check_number_of_args {
    ($fname:expr, $lv:expr,$expected:expr) => {
        if $lv.len() != $expected {
            return Err($crate::lerror::LRuntimeError::wrong_number_of_args(
                $fname,
                $lv,
                $expected..usize::MAX,
            ));
        }
    };
    ($lv:expr, $expected:expr) => {
        $crate::check_number_of_args!(function_name!(), $lv, $expected)
    };
}

#[macro_rules_attribute(lfn_extended!)]
pub fn test(p1: i64, p2: String) -> String {
    format!("test {}: {}", p1, p2)
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
