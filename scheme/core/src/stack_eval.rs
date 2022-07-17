use sompas_structs::lenv::LEnv;
use sompas_structs::lvalue::{LValue, Sym};
use std::sync::Arc;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Interruptibility {
    Interruptible,
    Unininterruptible,
    QuasiInterruptible,
}

pub struct ProcedureFrame {
    pub(crate) n: usize,
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
    Expand,
    Parse,
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

impl StackFrame {
    pub fn new(k: impl Into<StackKind>, i: Interruptibility) -> Self {
        Self {
            interruptibily: i,
            kind: k.into(),
        }
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

    pub fn quasiinterruptible(k: impl Into<StackKind>) -> Self {
        Self {
            interruptibily: Interruptibility::QuasiInterruptible,
            kind: k.into(),
        }
    }
}

pub enum StackKind {
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
    pub fn push_list(&mut self, mut list: Vec<StackFrame>) {
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
    pub fn last(&self) -> &LValue {
        self.inner.last().unwrap()
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
