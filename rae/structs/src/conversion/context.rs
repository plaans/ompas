use crate::acting_domain::OMPASDomain;
use crate::state::world_state::WorldStateSnapshot;
use crate::sym_table::domain::type_lattice::TypeLattice;
use crate::sym_table::domain::Domain;
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

        for command in domain.commands.values() {
            let domain = Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_COMMAND)),
                command.get_parameters().get_type_domain(),
                Box::new(Domain::nil()),
            );
            st.new_constant_symbol(command.get_label(), domain);
        }

        for task in domain.tasks.values() {
            let domain = Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_ABSTRACT_TASK)),
                task.get_parameters().get_type_domain(),
                Box::new(Domain::nil()),
            );
            st.new_constant_symbol(task.get_label(), domain);
        }

        for method in domain.methods.values() {
            let domain = Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_ABSTRACT_TASK)),
                method.get_parameters().get_type_domain(),
                Box::new(Domain::nil()),
            );
            st.new_constant_symbol(method.get_label(), domain);
        }

        for sf in domain.state_functions.values() {
            let domain = Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION)),
                sf.parameters.get_type_domain(),
                Box::new(sf.result.clone()),
            );
            st.new_constant_symbol(sf.get_label(), domain);
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
