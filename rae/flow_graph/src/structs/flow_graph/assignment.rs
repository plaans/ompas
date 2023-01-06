use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::GetVariables;
use crate::structs::domain::Domain;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use im::HashSet;

#[derive(Clone, Debug)]
pub struct Assignment {
    pub interval: Interval,
    pub result: AtomId,
    pub lit: Lit,
}

impl Assignment {
    pub fn get_computation(&self) -> &Lit {
        &self.lit
    }

    pub fn get_result(&self) -> AtomId {
        self.result
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_start(&self) -> AtomId {
        *self.interval.get_start()
    }

    pub fn get_end(&self) -> AtomId {
        *self.interval.get_end()
    }
}

impl GetVariables for Assignment {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut var = self.lit.get_variables();
        var.insert(self.result);
        var.union(self.interval.get_variables())
    }

    fn get_variables_in_domain(&self, _: &RefSymTable, _: &Domain) -> HashSet<AtomId> {
        todo!()
    }
}
