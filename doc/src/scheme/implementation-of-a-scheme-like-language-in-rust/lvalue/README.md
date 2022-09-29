---
description: Enum used to represent any value in scheme.
---

# LValue

The _LValue_ is the basic object used in our intrepreter to represent any kind of object. In rust it is an enumeration with the following kinds:

* **Number**: any kind of number. We distinquish three kinds of numbers: _Integer_, _Float_ and _Usize_. Usize is a particular type that can only be built by internal functions.
* **Symbol**: A string containing a unique word
* **String**: Mainly use for debugging and pass messages.
* **Character**: A simple character.
* **Nil**: Represent both boolean value false and the empty list
* **List**: A list LValues
* **Map**: A HashMap of LValues. This type is not originally in the basic structures of scheme. However it is of great interest for us to be implemented in the core functions, we added it the basic types.
* **Fn**: Pointer to functions with low level computation (i.e. here rust).
* **MutFn**: Same as Fn but it can modify objects.
* **AsyncFn**: Pointer to async functions. Here is an example of an async function:

```rust
use ::macro_rules_attribute::macro_rules_attribute;
// You need to add the following line in yout cargo.toml file
// macro_rules_attribute = "0.0.2"
use ompas_utils::dyn_async;

#[macro_rules_attribute(dyn_async!)]
async fn test<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> Result<LValue, LError> {
    Ok(LValue::Nil)
}
```

In case you would not want to use the macro, you can directly define the function with the following signature:

```rust
fn test<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> DynFut<'a> {
        Box::pin(async move { Ok(LValue::Nil) })
}
```

* **AsyncMutFn:** Same thing as an **AsyncFn**, but the context is mutable.
* **Future:** Contains a future Result\<LValue,LError>. A future is produced by an expression of the following kind:&#x20;

```
(async exp)
```

* **Lambda:** Lambda function that can be called using other kinds of LValues. A lambda is defined as follow:

```lisp
(define square (lambda (x) (* x x)))
; can also be defined like that:
(define (square x) (* x x))
```

