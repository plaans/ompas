use crate::structs::chronicle::GetVariables;
use crate::structs::domain::Domain;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::VarId;
use im::HashSet;

#[derive(Clone, Debug)]
pub struct Assignment {
    pub lit: Lit,
}

impl Assignment {
    pub fn get_computation(&self) -> &Lit {
        &self.lit
    }
}

impl GetVariables for Assignment {
    fn get_variables(&self) -> HashSet<VarId> {
        self.lit.get_variables()
    }

    fn get_variables_in_domain(&self, _: &RefSymTable, _: &Domain) -> HashSet<VarId> {
        todo!()
    }
}
