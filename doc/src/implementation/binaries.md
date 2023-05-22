---
description: Documentation on the struct LispInterpreter.
---

# Create binaries with the interpreter.

To define an interpreter, the struct LispInterpreter abstract a certain amount of internal mechanism to setup the environment. Here is a code example to setup the environment. It is recommended to always import **CtxUtils** soon enough, as it contains macros and lambdas definition of _let, let\*_ and other useful scheme helps.

```rust
let mut li = LispInterpreter::default();

let mut ctx_doc = CtxDoc::default();
let mut ctx_io = CtxIo::default();
let ctx_math = CtxMath::default();
let ctx_type = CtxType::default();
let ctx_utils = CtxUtils::default();
//Insert the doc for the different contexts.
ctx_doc.insert_doc(CtxIo::documentation());
ctx_doc.insert_doc(CtxMath::documentation());
ctx_doc.insert_doc(CtxType::documentation());
ctx_doc.insert_doc(CtxUtils::documentation());

//Add the sender of the channel.
ctx_io.add_communication(li.subscribe());
if let Some(pb) = &log {
    ctx_io.set_log_output(pb.clone().into());
}


li.import_namespace(CtxError::default());
li.import_namespace(ctx_utils);
li.import_namespace(ctx_doc);
li.import_namespace(ctx_io);
li.import_namespace(ctx_math);
li.import_namespace(ctx_type);

//println!("global ctxs: {}", ctxs);

li.set_config(LispInterpreterConfig::new(true));

li.run(log).await;
```
