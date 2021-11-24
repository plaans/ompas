use ompas_lisp::structs::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

type Sym = String;

type SymId = usize;

#[derive(Clone, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub enum SymType {
    Timepoint,
    Return,
    Object,
}

impl Display for SymType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SymType::Timepoint => write!(f, "timepoint"),
            SymType::Return => write!(f, "return"),
            SymType::Object => write!(f, "object"),
        }
    }
}

pub(crate) struct SymTable {
    symbols: Vec<Sym>,
    ids: HashMap<Sym, SymId>,
    symbol_types: HashMap<SymId, SymType>,
    types_number: HashMap<SymType, usize>,
}

impl Default for SymTable {
    fn default() -> Self {
        let mut types_number = HashMap::new();
        types_number.insert(SymType::Return, 0);
        types_number.insert(SymType::Timepoint, 0);
        types_number.insert(SymType::Object, 0);
        Self {
            symbols: vec![],
            ids: Default::default(),
            symbol_types: Default::default(),
            types_number,
        }
    }
}

impl SymTable {
    pub fn get(&self, id: &SymId) -> Option<&Sym> {
        self.symbols.get(*id)
    }

    pub fn id(&self, sym: &Sym) -> Option<&SymId> {
        self.ids.get(sym)
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn declare_new_return(&mut self) -> SymId {
        let n = self.types_number.get_mut(&SymType::Return).unwrap();
        let sym = format!("r_{}", n);
        *n = *n + 1 as usize;
        let id = self.symbols.len();
        self.symbols.push(sym.clone());
        self.ids.insert(sym, id);
        self.symbol_types.insert(id, SymType::Return);
        id
    }

    pub fn declare_new_interval(&mut self) -> Interval {
        let n = self.types_number.get_mut(&SymType::Timepoint).unwrap();
        let start = format!("t_{}", n);
        let end = format!("t_{}", *n + 1 as usize);
        *n = *n + 2 as usize;
        let id_1 = self.symbols.len();
        let id_2 = id_1 + 1;
        self.symbols.push(start.clone());
        self.symbols.push(end.clone());
        self.ids.insert(start, id_1);
        self.symbol_types.insert(id_1, SymType::Timepoint);
        self.ids.insert(end, id_2);
        self.symbol_types.insert(id_2, SymType::Timepoint);
        Interval {
            start: id_1,
            end: id_2,
        }
    }

    pub fn declare_new_object(&mut self, obj: Option<Sym>) -> SymId {
        let n = self.types_number.get_mut(&SymType::Object).unwrap();
        let sym = match obj {
            Some(s) => s,
            None => {
                format!("o_{}", n)
            }
        };
        *n = *n + 1 as usize;
        let id = self.symbols.len();
        self.symbols.push(sym.clone());
        self.ids.insert(sym, id);
        self.symbol_types.insert(id, SymType::Object);
        id
    }
}

trait FormatWithSymTable {
    fn format_with_sym_table(&self, st: &SymTable) -> String;
}

impl FormatWithSymTable for Chronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let get_sym = |id: &SymId| {
            st.get(id)
                .expect("error in the definition of the symbol_table")
        };

        let mut s = String::new();
        //name
        s.push_str("-name: ");
        for e in &self.name {
            s.push_str(get_sym(e).as_str());
            s.push(' ');
        }
        s.push('\n');
        //task
        s.push_str("-task: ");
        for e in &self.task {
            s.push_str(get_sym(e).as_str());
            s.push(' ');
        }
        s.push('\n');
        //variables
        s.push_str("-task: {");
        for (i, id) in self.task.iter().enumerate() {
            if i != 0 {
                s.push(',');
            }
            s.push_str(get_sym(id).as_str());
        }
        s.push_str("}\n");
        //conditions
        s.push_str("-conditon(s): {\n");
        for e in &self.conditions {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //effects
        s.push_str("-conditon(s): {\n");
        for e in &self.effects {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {\n");
        for e in &self.subtasks {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }
}

pub struct Chronicle {
    name: Vec<SymId>,
    task: Vec<SymId>,
    variables: Vec<SymId>,
    conditions: Vec<SV>,
    effects: Vec<SV>,
    subtasks: Vec<Expression>,
}

pub struct ExpressionChronicle {
    interval: Interval,
    value: SymId,
    chronicle: Chronicle,
    debug: LValue,
}

impl FormatWithSymTable for ExpressionChronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let get_sym = |id: &SymId| {
            st.get(id)
                .expect("error in the definition of the symbol_table")
        };
        let mut s = String::new();

        s.push_str(format!("{}", self.debug).as_str());

        s
    }
}
pub struct SV {
    interval: Interval,
    constraint: Constraint,
}

impl FormatWithSymTable for SV {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.constraint.format_with_sym_table(st)
        )
    }
}

pub enum Lit {
    Atom(SymId),
    LValue(LValue),
    Constraint(Box<Constraint>),
    Exp(Vec<Lit>),
}

impl FormatWithSymTable for Lit {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Lit::Atom(a) => st.symbols.get(*a).unwrap().clone(),
            Lit::Constraint(c) => c.format_with_sym_table(st),
            Lit::Exp(vec) => {
                let mut str = "(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format_with_sym_table(&st).as_str())
                }
                str
            }
            Lit::LValue(lv) => lv.to_string(),
        }
    }
}

pub enum Constraint {
    Eq(Lit, Lit),
    Neg(Lit),
    LT(Lit, Lit),
}

impl FormatWithSymTable for Constraint {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Constraint::Eq(l1, l2) => format!(
                "{} = {}",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::Neg(l1) => format!("! {}", l1.format_with_sym_table(st)),
            Constraint::LT(l1, l2) => format!(
                "{} < {}",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
        }
    }
}

pub struct Expression {
    interval: Interval,
    lit: Lit,
}

impl FormatWithSymTable for Expression {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.lit.format_with_sym_table(st)
        )
    }
}

pub struct Interval {
    start: SymId,
    end: SymId,
}

impl FormatWithSymTable for Interval {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "[{},{}]",
            st.symbols.get(self.start).unwrap(),
            st.symbols.get(self.end).unwrap()
        )
    }
}

pub struct Problem {}

type Action = Chronicle;
type Method = Chronicle;

struct Domain {
    actions: Vec<Action>,
    tasks: Vec<Lit>,
    methods: Vec<Method>,
}
