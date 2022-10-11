use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::sym_table::SymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, FormatWithParent, FormatWithSymTable, GetVariables};
use im::HashSet;

#[derive(Clone, Eq, PartialEq)]
pub struct Condition {
    pub interval: Interval,
    pub sv: Vec<AtomId>,
    pub value: AtomId,
}

impl Condition {
    pub fn get_start(&self) -> &AtomId {
        self.interval.start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.interval.end()
    }
}

impl FormatWithSymTable for Condition {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let sf = &self.sv[0];
        let params = &self.sv[1..];
        format!(
            "{} {}{} = {}",
            self.interval.format(st, sym_version),
            sf.format(st, sym_version),
            params.format(st, sym_version),
            self.value.format(st, sym_version),
        )
    }
}

impl FormatWithParent for Condition {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.interval.format_with_parent(st);
        self.sv.format_with_parent(st);
        self.value.format_with_parent(st);
    }
}

impl GetVariables for Condition {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashset = self.interval.get_variables();
        hashset.insert(self.value);
        self.sv.iter().for_each(|a| {
            hashset.insert(*a);
        });
        hashset
    }

    fn get_variables_of_type(&self, sym_table: &SymTable, atom_type: &AtomType) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap() == atom_type)
            .cloned()
            .collect()
    }
}
