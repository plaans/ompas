use sompas_structs::lenv::LEnv;
use sompas_structs::lvalue::{LValue, Sym};
use std::sync::Arc;

pub enum Interruptibility {
    Interruptible,
    Unininterruptible,
    QuasiInterruptible,
}

pub struct ProcedureFrame {
    pub(crate) n: usize,
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
    Lambda,
    Await,
    Eval,
    Expand,
    Parse,
}

impl From<CoreOperatorFrame> for StackFrame {
    fn from(c: CoreOperatorFrame) -> Self {
        StackFrame::CoreOperator(c)
    }
}

pub struct IfFrame {
    pub(crate) conseq: LValue,
    pub(crate) alt: LValue,
}

impl From<IfFrame> for StackFrame {
    fn from(i: IfFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::If(i))
    }
}

pub struct BeginFrame {
    pub(crate) n: usize,
}

impl From<BeginFrame> for StackFrame {
    fn from(b: BeginFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Begin(b))
    }
}

pub struct DoFrame {
    pub(crate) rest: Vec<LValue>,
}

impl From<DoFrame> for StackFrame {
    fn from(d: DoFrame) -> Self {
        Self::CoreOperator(CoreOperatorFrame::Do(d))
    }
}

pub struct DefineFrame {
    pub(crate) symbol: Arc<Sym>,
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
