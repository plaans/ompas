use crate::conversion::flow_graph_conversion::convert_into_flow_graph;
use crate::conversion::flow_graph_post_process::flow_graph_post_processing;
use crate::conversion::lvalue_pre_processing::pre_processing;
use crate::structs::chronicle::template::{ChronicleKind, ChronicleTemplate};
use crate::FlowGraph;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;

pub mod chronicle_conversion;
pub mod chronicle_post_processing;
pub mod flow_graph_conversion;
pub mod flow_graph_post_process;
pub mod lvalue_pre_processing;

pub async fn convert(lv: &LValue, env: &LEnv) -> Result<ChronicleTemplate, LRuntimeError> {
    let lv = pre_processing(lv, env).await?;

    //let sym_table = RefCell::new(SymTable::default());

    let mut graph = FlowGraph::default();

    let flow = convert_into_flow_graph(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    //flow_graph_post_processing(&mut graph)?;
    let mut ch = ChronicleTemplate::new("debug", ChronicleKind::Method, graph.sym_table.clone());
    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = lv;
    Ok(ch)

    //convert_method(&graph, graph.scope)
}
