use crate::lvalue::LValue;
use im::Vector;
use macro_rules_attribute::macro_rules_attribute;
extern crate proc_macro;
use proc_macro::TokenStream;

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
        LValue::Err(std::sync::Arc::new($x.to_string()))
    };
}

#[macro_export]
macro_rules! string {
    ($x:expr) => {
        LValue::String(std::sync::Arc::new($x.to_string()))
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
        $vi fn $fname($env: &$crate::lenv::LEnv, $arg: &im::Vector<$crate::lvalue::LValue>) -> $crate::lerror::LResult {
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

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::count!($($xs)*));
}

#[macro_export]
macro_rules! transform_arg {
    ($id:ident $count:literal $arg:ident $t:tt) => {
        let $arg: $t = <$t>::try_from(&$id[$count])?;
    };
}
#[macro_export]
macro_rules! check_args {
    ($count:literal $id:ident) => {};
    ($count:literal $id:ident $arg1:ident $t1:tt $(, $arg:ident $t:tt)*) => {
        $crate::transform_arg!($id $count $arg1 $t1);
        //let $arg1: $t1 = <$t1>::try_from(&args[0])?;
        $crate::check_args!($count $id $($arg $t),*);
    };
}
#[macro_export]
macro_rules! fn_result {
    ($r:ident) => {
        Ok(LValue::Nil)
    };
    ($r:ident LResult) => {
        $r
    }; /*($r:ident, std::result::Result<$o:ty,$e:ty>) => {};*/
    ($r:ident LValue) => {
        Ok($r)
    };
    ($r:ident $o:tt) => {
        Ok(LValue::from($r))
    };
}

#[macro_export]
macro_rules! lfn_extended {
(
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($env:ident : &LEnv $(, $arg:ident : $t:ty)*)  $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
    $( #[$attr] )*
    #[allow(unused_parens)]
    #[function_name::named]
    $pub
    fn $fname($env: &crate::lenv::LEnv, args : &im::Vector<LValue>) -> $crate::lerror::LResult
    {
        $crate::check_number_of_args!(args, $crate::count!($($arg)*));
        $crate::check_args!(0 args $($arg $t),*);
        let result: $($Ret)? = {|| {
            $($body)*
        }}();
        $crate::fn_result!(result $($Ret)?)
    }
);
/*    (
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($($arg:ident : $t:ty),*) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
        lfn_extended!{
            $( #[$attr] )* // includes doc strings
    $pub
    fn $fname(__env__: &LEnv $(,$arg : $t)*) $(-> $Ret)?
    {
        $($body)*
    }}
);*/
    (
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($env:ident : &LEnv, $arg:ident : &Vector<LValue>) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => ($( #[$attr] )* // includes doc strings
    $( #[$attr] )*
    #[allow(unused_parens)]
    #[function_name::named]
    $pub
    fn $fname($env: &crate::lenv::LEnv, $arg : &im::Vector<LValue>) -> $crate::lerror::LResult
    {
        let result: $($Ret)? = {|| {
            $($body)*
        }}();
        $crate::fn_result!(result $($Ret)?)
    }
);
    (
    $( #[$attr:meta] )* // includes doc strings
    $pub:vis
    fn $fname:ident($arg:ident: &Vector<LValue>) $(-> $Ret:ty)?
    {
        $($body:tt)*
    }
) => (
        lfn_extended!{
            $( #[$attr] )* // includes doc strings
    $pub
    fn $fname(__env__: &LEnv, $arg: &Vector<LValue>) $(-> $Ret)?
    {
        $($body)*
    }}
);
}

#[macro_export]
macro_rules! lerror {
    ($fname:expr, $msg:expr) => {
        $crate::lerror::LRuntimeError::new($fname, $msg)
    };
    ($msg:expr) => {
        $crate::lerror!(function_name!(), $msg)
    };
}

#[macro_export]
macro_rules! check_type {
    ($fname:expr, $lv:expr,$expected:expr) => {
        if $lv.get_kind() != $expected {
            return Err($crate::lerror::LRuntimeError::wrong_type(
                $fname, $lv, $expected,
            ));
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

#[macro_export]
macro_rules! wrong_n_args {
    ($fname:expr, $lv:expr,$expected:expr) => {
        $crate::lerror::LRuntimeError::wrong_number_of_args($fname, $lv, $expected..usize::MAX)
    };
    ($fname:expr, $lv:expr,$lw:tt..$up:tt$) => {
        $crate::lerror::LRuntimeError::wrong_number_of_args($fname, $lv, $lw..$up)
    };
    ($lv:expr, $low:tt $(..$up:tt)*) => {
        $crate::wrong_n_args!(function_name!(), $lv, $low $(..$up)*)
    };
}

#[macro_export]
macro_rules! wrong_type {
    ($fname:expr, $lv:expr,$expected:expr) => {
        $crate::lerror::LRuntimeError::wrong_type($fname, $lv, $expected)
    };
    ($lv:expr,$expected:expr) => {
        wrong_type!(function_name!(), $lv, $expected)
    };
}

/*#[test]
pub fn test() -> LValue {
    LValue::Nil
}*/
