use crate::language::{LIST, MAP};
use crate::structs::LValue;
use im::HashMap;

///Transform an object in Lisp command to reconstuct itself.
pub trait AsLiteral {
    fn as_command(&self) -> String;
}

impl AsLiteral for Vec<LValue> {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", LIST).as_str());
        for param in self {
            result.push_str(format!("{} ", param.as_command()).as_str());
        }
        result
    }
}

impl AsLiteral for HashMap<LValue, LValue> {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", MAP).as_str());
        for (key, value) in self.iter() {
            result.push_str("(list ");
            result.push_str(format!("{} {})", key.as_command(), value.as_command()).as_str())
        }
        result.push_str(")\n");
        result
    }
}

impl AsLiteral for LValue {
    fn as_command(&self) -> String {
        match self {
            LValue::List(l) => l.as_command(),
            LValue::String(s) => s.to_string(),
            LValue::Fn(f) => f.debug_label.clone(),
            LValue::MutFn(f) => f.debug_label.clone(),
            LValue::None => "none".to_string(),
            LValue::Symbol(s) => s.to_string(),
            LValue::Number(n) => n.to_string(),
            LValue::Bool(b) => b.to_string(),
            LValue::Lambda(_) => "".to_string(),
            LValue::Map(m) => m.as_command(),
            LValue::Quote(q) => q.to_string(),
            LValue::CoreOperator(c) => c.to_string(),
        }
    }
}
