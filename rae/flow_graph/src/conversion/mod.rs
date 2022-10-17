use crate::conversion::chronicle_conversion::convert_into_chronicle;
use crate::conversion::flow_graph_conversion::convert_into_flow_graph;
use crate::conversion::lvalue_pre_processing::pre_processing;
use crate::structs::chronicle::chronicle::ChronicleTemplate;
use crate::FlowGraph;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;

pub mod chronicle_conversion;
pub mod chronicle_post_processing;
pub mod flow_graph_conversion;
pub mod lvalue_pre_processing;

pub async fn convert(lv: &LValue, env: &LEnv) -> Result<ChronicleTemplate, LRuntimeError> {
    let lv = pre_processing(&lv, &env).await?;

    //let sym_table = RefCell::new(SymTable::default());

    let mut graph = FlowGraph::default();

    let scope = convert_into_flow_graph(&lv, &mut graph, &mut Default::default())?;
    graph.scope = scope;

    let ch = convert_into_chronicle(&graph, graph.scope);
    ch
}
