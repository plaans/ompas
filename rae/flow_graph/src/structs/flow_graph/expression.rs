use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable};
use crate::structs::flow_graph::scope::Scope;
use std::fmt::Write;
#[derive(Debug, Clone)]
pub enum Block {
    If(IfBlock),
}
impl FlatBindings for Block {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Block::If(if_block) => if_block.flat_bindings(st),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfBlock {
    pub(crate) cond: AtomId,
    pub(crate) true_result: AtomId,
    pub(crate) false_result: AtomId,
    pub(crate) true_branch: Scope,
    pub(crate) false_branch: Scope,
}

impl FlatBindings for IfBlock {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.cond.flat_bindings(st);
        self.true_result.flat_bindings(st);
        self.false_result.flat_bindings(st);
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Block),
    Err(Lit),
    Exec(Vec<AtomId>),
    Apply(Vec<AtomId>),
    Write(Vec<AtomId>),
    Read(Vec<AtomId>),
    Cst(Lit),
    Handle(AtomId),
    Await(AtomId),
}

impl Expression {
    pub fn apply(vec: Vec<AtomId>) -> Self {
        Self::Apply(vec)
    }

    pub fn write(vec: Vec<AtomId>) -> Self {
        Self::Write(vec)
    }

    pub fn read(vec: Vec<AtomId>) -> Self {
        Self::Read(vec)
    }
    pub fn exec(vec: Vec<AtomId>) -> Self {
        Self::Exec(vec)
    }

    pub fn cst(cst: impl Into<Lit>) -> Self {
        Self::Cst(cst.into())
    }

    pub fn err(err: Lit) -> Self {
        Self::Err(err)
    }
}

impl FormatWithSymTable for Expression {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            Expression::Apply(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "apply({})", args)
            }
            Expression::Write(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "write({})", args)
            }
            Expression::Read(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "read({})", args)
            }
            Expression::Cst(cst) => {
                write!(str, "cst({})", cst.format(st, sym_version))
            }
            Expression::Exec(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "exec({})", args)
            }
            Expression::Err(err) => {
                write!(str, "err({})", err.format(st, sym_version))
            }
            Expression::Block(block) => match block {
                Block::If(i) => {
                    write!(
                        str,
                        "if({},{},{})",
                        i.cond.format(st, sym_version),
                        i.true_result.format(st, sym_version),
                        i.false_result.format(st, sym_version),
                    )
                } /*Block::Handle(a) => {
                      write!(str, "handle({})", a.result.format(st, sym_version),)
                  }*/
            },
            Expression::Handle(h) => {
                write!(str, "handle({})", h.format(st, sym_version))
            }
            Expression::Await(a) => {
                write!(str, "await({})", a.format(st, sym_version))
            }
        }
        .unwrap();
        str
    }
}

impl FlatBindings for Expression {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Expression::Block(b) => b.flat_bindings(st),
            Expression::Err(e) => e.flat_bindings(st),
            Expression::Exec(e)
            | Expression::Apply(e)
            | Expression::Write(e)
            | Expression::Read(e) => e.flat_bindings(st),
            Expression::Cst(a) => a.flat_bindings(st),
            Expression::Handle(a) => a.flat_bindings(st),
            Expression::Await(a) => a.flat_bindings(st),
        }
    }
}
