use crate::acting_domain::OMPASDomain;
use crate::state::world_state::WorldStateSnapshot;
use crate::sym_table::domain::type_lattice::TypeLattice;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::*;
use sompas_structs::lenv::LEnv;

pub struct ConversionContext {
    pub state: WorldStateSnapshot,
    pub domain: OMPASDomain,
    pub st: RefSymTable,
    pub env: LEnv,
}

impl ConversionContext {
    pub fn new(
        domain: OMPASDomain,
        lattice: TypeLattice,
        state: WorldStateSnapshot,
        env: LEnv,
    ) -> Self {
        let mut st = SymTable::new_from(lattice);

        for command in domain.commands.keys() {
            st.new_constant_symbol(command, st.get_type_as_domain(TYPE_COMMAND));
        }

        for task in domain.tasks.keys() {
            st.new_constant_symbol(task, st.get_type_as_domain(TYPE_ABSTRACT_TASK));
        }

        for method in domain.methods.keys() {
            st.new_constant_symbol(method, st.get_type_as_domain(TYPE_METHOD));
        }

        for sf in domain.state_functions.keys() {
            st.new_constant_symbol(sf, st.get_type_as_domain(TYPE_STATE_FUNCTION));
        }

        for (t, instances) in state.instance.inner.iter() {
            for instance in instances {
                st.new_constant_symbol(instance, st.get_type_as_domain(t.to_string()));
            }
            st.new_constant_symbol(t, st.get_type_as_domain(TYPE_OBJECT_TYPE));
        }

        let st = st.into();

        ConversionContext {
            state,
            domain,
            st,
            env,
        }
    }
}
