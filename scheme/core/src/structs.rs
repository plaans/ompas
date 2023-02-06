use ompas_middleware::logger::LogClient;
use sompas_structs::lenv::LEnv;
use sompas_structs::list;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lvalue::{LValue, Sym};
use std::fmt::Display;
use std::sync::Arc;

pub trait Unstack {
    fn unstack(self, results: &mut Results) -> LValue;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Interruptibility {
    Interruptible,
    Unininterruptible,
}

pub struct ProcedureFrame {
    pub(crate) n: usize,
}

impl Unstack for ProcedureFrame {
    fn unstack(self, results: &mut Results) -> LValue {
        results.pop_n(self.n).into()
    }
}

impl From<ProcedureFrame> for StackKind {
    fn from(p: ProcedureFrame) -> Self {
        Self::Procedure(p)
    }
}

pub enum CoreOperatorFrame {
    If(IfFrame),
    Begin(BeginFrame),
    Do(DoFrame),
    Define(DefineFrame),
    Lambda,
    Await,
    Interrupt,
    Eval,
    EvalEnd,
    Enr,
    EnrEnd,
    Expand,
    Parse,
}

impl Unstack for CoreOperatorFrame {
    fn unstack(self, results: &mut Results) -> LValue {
        match self {
            CoreOperatorFrame::If(i) => {
                list![
                    LPrimitive::If.into(),
                    results.pop().unwrap(),
                    i.conseq,
                    i.alt
                ]
            }
            CoreOperatorFrame::Begin(b) => {
                let mut r = results.pop_n(b.n);
                let mut list = vec![LPrimitive::Begin.into()];
                list.append(&mut r);
                list.into()
            }
            CoreOperatorFrame::Do(mut df) => {
                let mut list = vec![LPrimitive::Do.into()];
                list.append(&mut df.results);
                list.push(results.pop().unwrap());
                list.append(&mut df.rest);
                list.into()
            }
            CoreOperatorFrame::Define(d) => {
                list!(
                    LPrimitive::Define.into(),
                    d.symbol.into(),
                    results.pop().unwrap()
                )
            }
            CoreOperatorFrame::Lambda => results.pop().unwrap(),
            CoreOperatorFrame::Await => {
                list!(LPrimitive::Await.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::Interrupt => {
                list!(LPrimitive::Interrupt.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::Eval => {
                list!(LPrimitive::Eval.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::Enr => {
                list!(LPrimitive::Enr.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::Expand => {
                list!(LPrimitive::Expand.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::Parse => {
                list!(LPrimitive::Parse.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::EvalEnd => {
                list!(LPrimitive::Eval.into(), results.pop().unwrap())
            }
            CoreOperatorFrame::EnrEnd => {
                list!(LPrimitive::Enr.into(), results.pop().unwrap())
            }
        }
    }
}

impl From<CoreOperatorFrame> for StackKind {
    fn from(c: CoreOperatorFrame) -> Self {
        StackKind::CoreOperator(c)
    }
}

pub struct IfFrame {
    pub(crate) conseq: LValue,
    pub(crate) alt: LValue,
}

impl From<IfFrame> for StackKind {
    fn from(i: IfFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::If(i))
    }
}

pub struct BeginFrame {
    pub(crate) n: usize,
}

impl From<BeginFrame> for StackKind {
    fn from(b: BeginFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Begin(b))
    }
}

pub struct DoFrame {
    pub(crate) results: Vec<LValue>,
    pub(crate) rest: Vec<LValue>,
}

impl From<DoFrame> for StackKind {
    fn from(d: DoFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Do(d))
    }
}

pub struct DefineFrame {
    pub(crate) symbol: Arc<Sym>,
}

impl From<DefineFrame> for StackKind {
    fn from(d: DefineFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Define(d))
    }
}

impl From<LValue> for StackKind {
    fn from(lv: LValue) -> Self {
        Self::NonEvaluated(lv)
    }
}

pub struct StackFrame {
    pub(crate) interruptibily: Interruptibility,
    pub(crate) kind: StackKind,
}

impl Unstack for StackFrame {
    fn unstack(self, results: &mut Results) -> LValue {
        self.kind.unstack(results)
    }
}

impl StackFrame {
    pub fn new(k: impl Into<StackKind>, i: Interruptibility) -> Self {
        Self {
            interruptibily: i,
            kind: k.into(),
        }
    }

    pub fn new_lvalue(mut k: LValue, mut i: Interruptibility) -> Self {
        while let LValue::List(ref list) = k {
            if list[0] == LValue::Primitive(LPrimitive::Uninterruptible) {
                i = Interruptibility::Unininterruptible;
                k = list[1].clone();
            } else if list[0] == LValue::Primitive(LPrimitive::Interruptible) {
                i = Interruptibility::Interruptible;
                k = list[1].clone();
            } else {
                break;
            }
        }
        Self::new(k, i)
    }

    pub fn uninterruptible(k: impl Into<StackKind>) -> Self {
        Self {
            interruptibily: Interruptibility::Unininterruptible,
            kind: k.into(),
        }
    }

    pub fn interruptible(k: impl Into<StackKind>) -> Self {
        Self {
            interruptibily: Interruptibility::Interruptible,
            kind: k.into(),
        }
    }
}

pub enum StackKind {
    NonEvaluated(LValue),
    Procedure(ProcedureFrame),
    CoreOperator(CoreOperatorFrame),
}

impl Unstack for StackKind {
    fn unstack(self, results: &mut Results) -> LValue {
        match self {
            StackKind::NonEvaluated(lv) => lv,
            StackKind::Procedure(p) => p.unstack(results),
            StackKind::CoreOperator(co) => co.unstack(results),
        }
    }
}

pub struct ScopeCollection<'a> {
    root_env: &'a mut LEnv,
    inner: Vec<LEnv>,
}

impl<'a> ScopeCollection<'a> {
    pub fn new(root_env: &'a mut LEnv) -> Self {
        Self {
            root_env,
            inner: vec![],
        }
    }

    pub fn new_defined_scope(&mut self, env: LEnv) {
        self.inner.push(env)
    }

    pub fn new_scope(&mut self) {
        match self.inner.last() {
            Some(last) => self.inner.push(last.clone()),
            None => self.inner.push(self.root_env.clone()),
        }
    }

    pub fn revert_scope(&mut self) {
        self.inner.pop();
    }

    pub fn get_last(&self) -> &LEnv {
        if self.inner.is_empty() {
            self.root_env
        } else {
            self.inner.last().unwrap()
        }
    }

    pub fn get_last_mut(&mut self) -> &mut LEnv {
        if self.inner.is_empty() {
            self.root_env
        } else {
            self.inner.last_mut().unwrap()
        }
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
    pub fn push_list(&mut self, mut list: Vec<StackFrame>) {
        list.reverse();
        for e in list {
            self.inner.push(e)
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
    pub fn last(&self) -> Option<&LValue> {
        self.inner.last()
    }

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

#[derive(Default)]
pub struct LDebug {
    inner: Vec<String>,
    pub log: LogClient,
}

impl LDebug {
    pub fn push(&mut self, i: Interruptibility, s: impl Display) {
        self.inner.push(format!(
            "{}: {}",
            match i {
                Interruptibility::Interruptible => "i",
                Interruptibility::Unininterruptible => "u",
            },
            s
        ))
    }

    pub fn pop(&mut self) {
        self.inner.pop();
    }

    pub async fn log_last_result(&mut self, results: &Results) {
        //if get_debug() {
        self.log
            .debug(format!(
                "{} => {}",
                self.inner.pop().unwrap(),
                results.last().unwrap()
            ))
            .await
        //}
    }
}
