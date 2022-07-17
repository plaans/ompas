use crate::get_debug;
use sompas_structs::function::{LAsyncFn, LFn};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::wrong_type;
use std::collections::VecDeque;
use std::sync::Arc;

pub enum Interruptibility {
    Interruptible,
    Unininterruptible,
    QuasiInterruptible,
}

pub struct ProcedureFrame {
    n: usize,
}

impl From<ProcedureFrame> for StackFrame {
    fn from(p: ProcedureFrame) -> Self {
        Self::Procedure(p)
    }
}

pub enum CoreOperatorFrame {
    If(IfFrame),
    Begin(BeginFrame),
    Do(DoFrame),
    Define(DefineFrame),
    Await,
}

pub struct IfFrame {
    conseq: LValue,
    alt: LValue,
}

impl From<IfFrame> for StackFrame {
    fn from(i: IfFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::If(i))
    }
}

pub struct BeginFrame {
    n: usize,
}

impl From<BeginFrame> for StackFrame {
    fn from(b: BeginFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Begin(b))
    }
}

pub struct DoFrame {
    rest: Vec<LValue>,
}

impl From<DoFrame> for StackFrame {
    fn from(d: DoFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Do(d))
    }
}

pub struct DefineFrame {
    symbol: Arc<Sym>,
}

impl From<DefineFrame> for StackFrame {
    fn from(d: DefineFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Define(d))
    }
}

impl From<LValue> for StackFrame {
    fn from(lv: LValue) -> Self {
        Self::NonEvaluated(lv)
    }
}

pub enum StackFrame {
    NonEvaluated(LValue),
    Procedure(ProcedureFrame),
    CoreOperator(CoreOperatorFrame),
}

#[derive(Default)]
pub struct ScopeCollection {
    inner: Vec<LEnv>,
}

impl ScopeCollection {
    pub fn new(root: LEnv) -> Self {
        Self { inner: vec![root] }
    }

    pub fn new_defined_scope(&mut self, env: LEnv) {
        self.inner.push(env)
    }

    pub fn new_scope(&mut self) {
        match self.inner.last() {
            Some(last) => self.inner.push(last.clone()),
            None => self.inner.push(LEnv::default()),
        }
    }

    pub fn revert_scope(&mut self) {}

    pub fn get_last(&self) -> Option<&LEnv> {
        self.inner.last()
    }

    pub fn get_last_mut(&mut self) -> Option<&mut LEnv> {
        self.inner.last_mut()
    }
}

#[derive(Default)]
pub struct EvalStack {
    inner: Vec<StackFrame>,
}

impl EvalStack {
    pub fn push(&mut self, f: StackFrame) {
        self.inner.push(f);
    }
    pub fn push_slice(&mut self, slice: &[LValue]) {
        let mut list = slice.to_vec();
        list.reverse();
        for e in list {
            self.inner.push(e.into())
        }
    }

    pub fn pop(&mut self) -> Option<StackFrame> {
        self.inner.pop()
    }
}

#[derive(Default)]
pub struct Results {
    inner: Vec<LValue>,
}

impl Results {
    pub fn push(&mut self, lv: LValue) {
        self.inner.push(lv);
    }

    pub fn pop(&mut self) -> Option<LValue> {
        self.inner.pop()
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<LValue> {
        self.inner.split_off(self.inner.len() - n)
    }
}

pub async fn non_recursive_eval(lv: LValue, env: LEnv) -> LResult {
    //let level: usize = 0;
    let mut queue: EvalStack = Default::default();
    queue.push(lv.into());

    let mut scopes: ScopeCollection = ScopeCollection::new(env.clone());
    let mut results: Results = Default::default();

    loop {
        let current = match queue.pop() {
            Some(lv) => lv,
            None => break,
        };

        let env = scopes.get_last_mut().unwrap();

        match current {
            StackFrame::NonEvaluated(lv) => match lv {
                LValue::List(list) => {
                    let mut list = list.as_slice();
                    let proc = &list[0];
                    let args = &list[1..];
                    if let LValue::CoreOperator(co) = proc {
                        match co {
                            LCoreOperator::Define => {
                                match &args[0] {
                                    LValue::Symbol(s) => {
                                        queue.push(DefineFrame { symbol: s.clone() }.into());
                                        queue.push(args[1].clone().into());
                                    }
                                    lv => return Err(wrong_type!("eval", lv, KindLValue::Symbol)),
                                };
                            }
                            LCoreOperator::DefLambda => {
                                let params = match &args[0] {
                                    LValue::List(list) => {
                                        let mut vec_sym = Vec::new();
                                        for val in list.iter() {
                                            match val {
                                                LValue::Symbol(s) => vec_sym.push(s.clone()),
                                                lv => {
                                                    return Err(wrong_type!(
                                                        "eval",
                                                        &lv,
                                                        KindLValue::Symbol
                                                    ))
                                                }
                                            }
                                        }
                                        vec_sym.into()
                                    }
                                    LValue::Symbol(s) => s.clone().into(),
                                    LValue::Nil => LambdaArgs::Nil,
                                    lv => {
                                        return Err(LRuntimeError::not_in_list_of_expected_types(
                                            "eval",
                                            lv,
                                            vec![KindLValue::List, KindLValue::Symbol],
                                        ))
                                    }
                                };
                                let body = &args[1];
                                results.push(LValue::Lambda(LLambda::new(
                                    params,
                                    body.clone(),
                                    env.get_symbols(),
                                )));
                            }
                            LCoreOperator::If => {
                                let stack = IfFrame {
                                    conseq: args[1].clone(),
                                    alt: args[2].clone(),
                                };

                                queue.push(stack.into());
                                queue.push(args[1].clone().into());
                            }
                            LCoreOperator::Quote => results.push(args[0].clone()),
                            LCoreOperator::QuasiQuote => {
                                panic!("quasiquote not allowed")
                            }
                            LCoreOperator::UnQuote => {
                                panic!("unquote not allowed")
                            }
                            LCoreOperator::DefMacro => {
                                panic!("defmacro not allowed")
                            }
                            LCoreOperator::Begin => {
                                let stack = BeginFrame { n: args.len() };
                                queue.push(stack.into());
                                queue.push_slice(args);
                            }
                            LCoreOperator::Do => {
                                let stack = DoFrame {
                                    rest: args[1..].to_vec(),
                                };

                                queue.push(stack.into());
                                queue.push(args[0].clone().into())
                            }
                            /*LCoreOperator::Async => {
                                let lvalue = args[0].clone();
                                let mut new_env = env.clone();

                                /*let future: LValue =
                                tokio::spawn(
                                    async move { eval(&lvalue, &mut new_env, &mut ctxs).await },
                                )
                                .await
                                .unwrap()?;*/

                                let future: LValue =
                                    (Box::pin(async move { eval(&lvalue, &mut new_env).await })
                                        as FutureResult)
                                        .into();
                                let future_2 = future.clone();
                                tokio::spawn(async move {
                                    #[allow(unused_must_use)]
                                    if let LValue::Future(future_2) = future_2 {
                                        future_2.await;
                                    }
                                });

                                return Ok(future);
                            }
                            LCoreOperator::Await => {
                                //println!("awaiting on async evaluation");
                                queue.push(CoreOperatorFrame::Await);
                                queue.push(args[0].clone().into());
                            }*/
                            _ => panic!("not yet supported in stack-based interpreter"),
                        }
                    } else {
                        queue.push(ProcedureFrame { n: list.len() }.into());
                        queue.push_slice(&list);
                    }
                }
                _ => results.push(lv.into()),
            },
            StackFrame::Procedure(pro) => {
                let mut exps: Vec<LValue> = results.pop_n(pro.n);
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        queue.push(l.get_body().clone().into());
                        let temp_env = l.get_new_env(env.clone(), args)?;
                        scopes.new_defined_scope(temp_env);
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = fun.call(env, args)?;
                        results.push(r_lvalue);
                    }
                    LValue::AsyncFn(fun) => {
                        let r_lvalue = fun.call(env, args).await?;
                        results.push(r_lvalue);
                    }
                    lv => {
                        return Err(wrong_type!("eval", lv, KindLValue::Fn));
                    }
                }
            }
            StackFrame::CoreOperator(cos) => match cos {
                CoreOperatorFrame::Define(d) => {
                    let env = scopes.get_last_mut().unwrap();
                    let value = results.pop().unwrap();
                    env.insert(d.symbol.as_ref(), value);
                    results.push(LValue::Nil);
                }
                CoreOperatorFrame::If(i) => {
                    let result = results.pop().unwrap();

                    match result {
                        LValue::True => queue.push(i.conseq.into()),
                        LValue::Nil => queue.push(i.alt.into()),
                        lv => return Err(wrong_type!("eval", &lv, KindLValue::Bool)),
                    };
                }
                CoreOperatorFrame::Do(mut df) => {
                    if !df.rest.is_empty() {
                        let result = results.pop().unwrap();
                        if matches!(result, LValue::Err(_)) {
                            results.push(result)
                        } else {
                            let next = df.rest.remove(0);
                            queue.push(df.into());
                            queue.push(next.into())
                        }
                    }
                }
                /*CoreOperatorFrame::Await => {
                    let f = results.pop().unwrap();
                    if let LValue::AsyncHandler(a) = future {
                        results.push(a.await)
                    } else {
                        return Err(wrong_type!(EVAL, &future, KindLValue::Future));
                    };
                }*/
                _ => panic!("not yet supported"),
            },
        }
    }
    Ok(results.pop().unwrap())
}
