use crate::NodeId;

pub struct FutureTable {
    inner: im::HashMap<NodeId, NodeId>,
}
