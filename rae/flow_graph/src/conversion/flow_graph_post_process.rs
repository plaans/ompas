use crate::conversion::chronicle_post_processing::bind_atoms;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::FlatBindings;
use crate::{Expression, FlowGraph};
use sompas_structs::lruntimeerror::LRuntimeError;

pub fn flow_graph_post_processing(graph: &mut FlowGraph) -> Result<(), LRuntimeError> {
    let mut next = Some(graph.scope.start);
    let mut sym_table = graph.sym_table.clone();

    while let Some(id) = next {
        let vertice = graph.get(&id).unwrap();
        next = vertice.child;
        if let Expression::Cst(lit) = &vertice.computation {
            if let Lit::Atom(a) = lit {
                bind_atoms(&a, &vertice.result, &mut sym_table)?;
                let mut scope = graph.scope;
                graph.remove(&id, &mut scope);
                graph.scope = scope;
            }
        } else if let Expression::Block(b) = &vertice.computation {
        }
    }

    graph.flat_bindings(&sym_table);
    Ok(())
}
