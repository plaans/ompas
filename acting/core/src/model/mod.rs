use crate::model::acting_domain::OMPASDomain;
use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use ompas_language::sym_table::{TYPE_ABSTRACT_TASK, TYPE_COMMAND, TYPE_STATE_FUNCTION};

pub mod acting_domain;
pub mod chronicle;
pub mod process_ref;
pub mod sym_domain;
pub mod sym_table;

pub fn add_domain_symbols(st: &RefSymTable, domain: &OMPASDomain) {
    for command in domain.commands.values() {
        let domain = Domain::Application(
            Box::new(st.get_type_as_domain(TYPE_COMMAND).unwrap()),
            command.get_parameters().get_type_domain(),
            Box::new(Domain::nil()),
        );
        st.new_constant_symbol(command.get_label(), domain);
    }

    for task in domain.tasks.values() {
        let domain = Domain::Application(
            Box::new(st.get_type_as_domain(TYPE_ABSTRACT_TASK).unwrap()),
            task.get_parameters().get_type_domain(),
            Box::new(Domain::nil()),
        );
        st.new_constant_symbol(task.get_label(), domain);
    }

    for method in domain.methods.values() {
        let domain = Domain::Application(
            Box::new(st.get_type_as_domain(TYPE_ABSTRACT_TASK).unwrap()),
            method.get_parameters().get_type_domain(),
            Box::new(Domain::nil()),
        );
        st.new_constant_symbol(method.get_label(), domain);
    }

    for sf in domain.state_functions.values() {
        let domain = Domain::Application(
            Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION).unwrap()),
            sf.parameters.get_type_domain(),
            Box::new(sf.result.clone()),
        );
        st.new_constant_symbol(sf.get_label(), domain);
    }

    /*for (t, instances) in state.instance.inner.iter() {
        for instance in instances {
            st.new_constant_symbol(instance, st.get_type_as_domain(t.to_string()).unwrap());
        }
        st.new_constant_symbol(t, st.get_type_as_domain(TYPE_OBJECT_TYPE).unwrap());
    }*/
}
