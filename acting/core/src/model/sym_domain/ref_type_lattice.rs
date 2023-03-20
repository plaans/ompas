use crate::model::sym_domain::simple_type::SimpleType;
use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_domain::{Domain, TypeId};
use std::collections::HashSet;
use std::fmt::Display;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone, Debug)]
pub struct RefTypeLattice(Arc<RwLock<TypeLattice>>);

impl RefTypeLattice {
    pub async fn get_lattice(&self) -> TypeLattice {
        self.0.read().await.clone()
    }

    pub async fn format_type(&self, id: &TypeId) -> String {
        self.0.read().await.format_type(id)
    }

    pub async fn get_type_id(&self, r#type: impl Display) -> Option<TypeId> {
        self.0.read().await.get_type_id(r#type).cloned()
    }

    pub async fn get_parents(&self, id: &TypeId) -> Vec<TypeId> {
        self.0.read().await.get_parent(id)
    }

    pub async fn add_type(&self, r#type: impl Into<SimpleType>, parents: Vec<TypeId>) -> TypeId {
        self.0.write().await.add_type(r#type, parents)
    }

    pub async fn add_parent(&self, id_type: TypeId, id_parent: TypeId) {
        self.0.write().await.add_parent(id_type, id_parent)
    }

    pub async fn get_all_childs(&self, id: &TypeId) -> Vec<TypeId> {
        self.0.read().await.get_all_childs(id)
    }

    pub async fn get_all_parents(&self, id: &TypeId) -> Vec<TypeId> {
        self.0.read().await.get_all_parents(id)
    }

    pub async fn get_decomposition(&self, id: &TypeId) -> Vec<TypeId> {
        self.0.read().await.get_decomposition(id).clone()
    }

    pub async fn contained_in(&self, d1: &Domain, d2: &Domain) -> bool {
        self.0.read().await.contained_in(d1, d2)
    }

    pub(crate) async fn __meet(&self, t1: &Domain, t2: &Domain) -> Domain {
        self.0.read().await.__meet(t1, t2)
    }

    pub(crate) async fn __union(&self, t1: &Domain, t2: &Domain) -> Domain {
        self.0.read().await.__union(t1, t2)
    }

    pub(crate) async fn __substract(&self, t1: &Domain, t2: &Domain) -> Domain {
        self.0.read().await.__substract(t1, t2)
    }

    #[allow(dead_code)]
    pub(crate) async fn simplify_union(&self, set: HashSet<Domain>) -> Domain {
        self.0.read().await.simplify_union(set)
    }

    pub async fn export_dot(&self) -> String {
        self.0.read().await.export_dot()
    }

    pub fn new() -> Self {
        RefTypeLattice(Arc::new(RwLock::new(TypeLattice::new())))
    }
}
