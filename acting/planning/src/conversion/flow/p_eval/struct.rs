use im::{hashset, HashSet};
use ompas_language::exec::platform::EXEC_COMMAND;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::exec::resource::{ACQUIRE, RELEASE};
use ompas_language::exec::state::WAIT_FOR;
use sompas_language::time::SLEEP;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::{list, symbol};
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct PLEnv {
    pub env: LEnv,
    pub unpure_binding: im::HashMap<String, PLValue>,
    pub pc: PConfig,
}

impl PLEnv {
    pub fn add_unpure_binding(&mut self, symbol: String) {
        self.unpure_binding
            .insert(symbol.to_string(), PLValue::unpure(symbol.into()));
    }

    pub fn get_unpure(&self, symbol: &str) -> Option<&PLValue> {
        self.unpure_binding.get(symbol)
    }
    pub fn get_env(&self) -> &LEnv {
        &self.env
    }
    pub fn get_p_config(&self) -> &PConfig {
        &self.pc
    }
}

#[derive(Clone, Debug)]
pub struct PLValue {
    pub(crate) lvalue: LValue,
    pure: bool,
}

impl PLValue {
    pub fn is_pure(&self) -> bool {
        self.pure
    }

    pub fn get_lvalue(&self) -> LValue {
        self.lvalue.clone()
    }

    pub fn pure(lv: LValue) -> PLValue {
        PLValue {
            lvalue: lv,
            pure: true,
        }
    }

    pub fn unpure(lv: LValue) -> PLValue {
        PLValue {
            lvalue: lv,
            pure: false,
        }
    }

    pub fn lvalue_as_quote(self) -> LValue {
        match self.pure {
            false => self.lvalue,
            true => match self.lvalue {
                LValue::Nil | LValue::True | LValue::Number(_) => self.lvalue,
                _ => list![LPrimitive::Quote.into(), self.lvalue],
            },
        }
    }
}

impl Default for PLValue {
    fn default() -> Self {
        Self {
            lvalue: LValue::Nil,
            pure: true,
        }
    }
}

impl Display for PLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.pure {
            true => write!(f, "{}", self.lvalue),
            false => write!(f, "u:{}", self.lvalue),
        }
    }
}

impl From<LValue> for PLValue {
    fn from(value: LValue) -> Self {
        Self {
            lvalue: value,
            pure: true,
        }
    }
}

impl From<PLValue> for LValue {
    fn from(pl: PLValue) -> Self {
        pl.lvalue
    }
}

impl From<&PLValue> for LValue {
    fn from(pl: &PLValue) -> Self {
        pl.clone().into()
    }
}

#[derive(Default, Clone)]
pub struct ParameterTable {
    pub inner: im::HashMap<String, PLValue>,
}

impl ParameterTable {
    pub fn add_param(&mut self, param: String) {
        self.inner
            .insert(param.to_string(), PLValue::unpure(symbol!(param.into())));
    }
    pub fn add_instantiated(&mut self, param: String, value: LValue) {
        self.inner.insert(param.to_string(), PLValue::pure(value));
    }

    pub fn try_get_param(&self, param: &str) -> Option<&PLValue> {
        self.inner.get(param)
    }
}

#[derive(Clone)]
pub struct PConfig {
    pub avoid: HashSet<String>,
    pub p_table: ParameterTable,
}

impl Default for PConfig {
    fn default() -> Self {
        Self {
            avoid: hashset![
                EXEC_COMMAND.to_string(),
                EXEC_TASK.to_string(),
                SLEEP.to_string(),
                WAIT_FOR.to_string(),
                ACQUIRE.to_string(),
                RELEASE.to_string()
            ],
            p_table: Default::default(),
        }
    }
}

use ompas_middleware::logger::LogClient;
use sompas_structs::lenv::LEnv;
use std::sync::Arc;

pub trait PUnstack {
    fn unstack(self, results: &mut PResults) -> PLValue;
}

pub struct PProcedureFrame {
    pub(crate) n: usize,
}

impl PUnstack for PProcedureFrame {
    fn unstack(self, results: &mut PResults) -> PLValue {
        PLValue::unpure(
            results
                .pop_n(self.n)
                .iter()
                .map(|plv| plv.clone().lvalue)
                .collect::<Vec<LValue>>()
                .into(),
        )
    }
}

impl From<PProcedureFrame> for PStackFrame {
    fn from(p: PProcedureFrame) -> Self {
        Self::Procedure(p)
    }
}

pub enum PCoreOperatorFrame {
    If(PIfFrame),
    Begin(PBeginFrame),
    Do(PDoFrame),
    Define(PDefineFrame),
    Lambda,
    Await,
    Eval,
    EvalEnd,
    Enr,
    EnrEnd,
    Expand,
    Parse,
    Async,
}

impl PUnstack for PCoreOperatorFrame {
    fn unstack(self, results: &mut PResults) -> PLValue {
        PLValue::unpure(match self {
            PCoreOperatorFrame::If(i) => {
                list![
                    LPrimitive::If.into(),
                    results.pop().unwrap().lvalue,
                    i.conseq,
                    i.alt
                ]
            }
            PCoreOperatorFrame::Begin(b) => {
                let mut r = results.pop_n(b.n).drain(..).map(|plv| plv.lvalue).collect();
                let mut list = vec![LPrimitive::Begin.into()];
                list.append(&mut r);
                list.into()
            }
            PCoreOperatorFrame::Do(mut df) => {
                let mut list = vec![LPrimitive::Do.into()];
                list.append(&mut df.results.drain(..).map(|plv| plv.lvalue).collect());
                list.push(results.pop().unwrap().lvalue);
                list.append(&mut df.rest);
                list.into()
            }
            PCoreOperatorFrame::Define(d) => {
                list!(
                    LPrimitive::Define.into(),
                    d.symbol.into(),
                    results.pop().unwrap().lvalue_as_quote()
                )
            }
            PCoreOperatorFrame::Lambda => results.pop().unwrap().lvalue,
            PCoreOperatorFrame::Await => {
                list!(LPrimitive::Await.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::Eval => {
                list!(LPrimitive::Eval.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::Enr => {
                list!(LPrimitive::Enr.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::Expand => {
                list!(LPrimitive::Expand.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::Parse => {
                list!(LPrimitive::Parse.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::EvalEnd => {
                list!(LPrimitive::Eval.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::EnrEnd => {
                list!(LPrimitive::Enr.into(), results.pop().unwrap().lvalue)
            }
            PCoreOperatorFrame::Async => {
                list!(LPrimitive::Async.into(), results.pop().unwrap().lvalue)
            }
        })
    }
}

impl From<PCoreOperatorFrame> for PStackFrame {
    fn from(c: PCoreOperatorFrame) -> Self {
        PStackFrame::CoreOperator(c)
    }
}

pub struct PIfFrame {
    pub(crate) conseq: LValue,
    pub(crate) alt: LValue,
}

impl From<PIfFrame> for PStackFrame {
    fn from(i: PIfFrame) -> Self {
        Self::CoreOperator(PCoreOperatorFrame::If(i))
    }
}

pub struct PBeginFrame {
    pub(crate) n: usize,
}

impl From<PBeginFrame> for PStackFrame {
    fn from(b: PBeginFrame) -> Self {
        Self::CoreOperator(PCoreOperatorFrame::Begin(b))
    }
}

pub struct PDoFrame {
    pub(crate) results: Vec<PLValue>,
    pub(crate) rest: Vec<LValue>,
    pub(crate) pure: bool,
}

impl From<PDoFrame> for PStackFrame {
    fn from(d: PDoFrame) -> Self {
        Self::CoreOperator(PCoreOperatorFrame::Do(d))
    }
}

pub struct PDefineFrame {
    pub(crate) symbol: Arc<Sym>,
}

impl From<PDefineFrame> for PStackFrame {
    fn from(d: PDefineFrame) -> Self {
        Self::CoreOperator(PCoreOperatorFrame::Define(d))
    }
}

impl From<LValue> for PStackFrame {
    fn from(lv: LValue) -> Self {
        Self::NonEvaluated(lv)
    }
}

pub enum PStackFrame {
    NonEvaluated(LValue),
    Procedure(PProcedureFrame),
    CoreOperator(PCoreOperatorFrame),
}

impl PUnstack for PStackFrame {
    fn unstack(self, results: &mut PResults) -> PLValue {
        match self {
            PStackFrame::NonEvaluated(lv) => PLValue::unpure(lv),
            PStackFrame::Procedure(p) => p.unstack(results),
            PStackFrame::CoreOperator(co) => co.unstack(results),
        }
    }
}

pub struct PScopeCollection<'a> {
    root_env: &'a mut PLEnv,
    inner: Vec<PLEnv>,
}

impl<'a> PScopeCollection<'a> {
    pub fn new(root_env: &'a mut PLEnv) -> Self {
        Self {
            root_env,
            inner: vec![],
        }
    }

    pub fn new_defined_scope(&mut self, env: PLEnv) {
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

    pub fn get_last(&self) -> &PLEnv {
        if self.inner.is_empty() {
            self.root_env
        } else {
            self.inner.last().unwrap()
        }
    }

    pub fn get_last_mut(&mut self) -> &mut PLEnv {
        if self.inner.is_empty() {
            self.root_env
        } else {
            self.inner.last_mut().unwrap()
        }
    }
}

#[derive(Default)]
pub struct PEvalStack {
    inner: Vec<PStackFrame>,
}

impl PEvalStack {
    pub fn push(&mut self, f: impl Into<PStackFrame>) {
        self.inner.push(f.into());
    }
    pub fn push_list(&mut self, mut list: Vec<PStackFrame>) {
        list.reverse();
        for e in list {
            self.inner.push(e)
        }
    }

    pub fn pop(&mut self) -> Option<PStackFrame> {
        self.inner.pop()
    }
}

#[derive(Default)]
pub struct PResults {
    inner: Vec<PLValue>,
}

impl PResults {
    pub fn last(&self) -> Option<&PLValue> {
        self.inner.last()
    }

    pub fn push(&mut self, lv: PLValue) {
        self.inner.push(lv);
    }

    pub fn pop(&mut self) -> Option<PLValue> {
        self.inner.pop()
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<PLValue> {
        self.inner.split_off(self.inner.len() - n)
    }
}

#[derive(Default)]
pub struct PLDebug {
    inner: Vec<String>,
    pub log: LogClient,
}

impl PLDebug {
    pub fn push(&mut self, s: impl Display) {
        self.inner.push(s.to_string())
    }

    pub fn pop(&mut self) {
        self.inner.pop();
    }

    pub async fn log_last_result(&mut self, results: &PResults) {
        //if get_debug() {
        self.log
            .trace(format!(
                "{} => {}",
                self.inner.pop().unwrap(),
                results.last().unwrap()
            ))
            .await
        //}
    }
}
