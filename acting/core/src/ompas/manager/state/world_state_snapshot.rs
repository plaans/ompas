use crate::ompas::manager::state::instance::InstanceCollection;
use crate::ompas::manager::state::partial_state::PartialState;
use crate::ompas::manager::state::StateType;
use sompas_core::modules::map::union_map;
use sompas_structs::lvalue::LValue;

#[derive(Default, Clone)]
pub struct WorldState {
    pub r#static: PartialState,
    pub dynamic: PartialState,
    pub inner_static: PartialState,
    pub inner_dynamic: PartialState,
}

impl WorldState {
    pub fn clear(&mut self) {
        self.r#static.inner.clear();
        self.dynamic.inner.clear();
        self.inner_static.inner.clear();
        self.inner_dynamic.inner.clear();
    }
}

#[derive(Default, Clone)]
pub struct WorldStateSnapshot {
    pub r#static: PartialState,
    pub dynamic: PartialState,
    pub inner_static: PartialState,
    pub inner_dynamic: PartialState,
    pub instance: InstanceCollection,
}

impl WorldStateSnapshot {
    pub fn get_state(&self, _type: Option<StateType>) -> PartialState {
        match _type {
            None => self.inner_static.union(
                &self.inner_dynamic.union(
                    &self.r#static.union(
                        &self
                            .dynamic
                            .union(&PartialState::from(self.instance.clone())),
                    ),
                ),
            ),
            Some(_type) => match _type {
                StateType::Static => self.r#static.union(&self.inner_static.clone()),
                StateType::Dynamic => self.dynamic.union(&self.inner_dynamic.clone()),
                StateType::Instance => self.instance.clone().into(),
                StateType::InnerStatic => self.inner_static.clone(),
                StateType::InnerDynamic => self.inner_dynamic.clone(),
            },
        }
    }

    pub fn absorb(&mut self, other: Self) {
        self.inner_static = self.inner_static.union(&other.inner_static);
        self.inner_dynamic = self.inner_dynamic.union(&other.inner_dynamic);
        self.r#static = self.r#static.union(&other.r#static);
        self.dynamic = self.dynamic.union(&other.dynamic);
    }
}

impl From<WorldStateSnapshot> for LValue {
    fn from(r: WorldStateSnapshot) -> Self {
        let env = Default::default();
        union_map(
            &env,
            &[
                union_map(
                    &env,
                    &[
                        union_map(
                            &env,
                            &[
                                PartialState::from(r.instance).into_map(),
                                r.dynamic.into_map(),
                            ],
                        )
                        .unwrap(),
                        r.r#static.into_map(),
                    ],
                )
                .unwrap(),
                union_map(
                    &env,
                    &[r.inner_static.into_map(), r.inner_dynamic.into_map()],
                )
                .unwrap(),
            ],
        )
        .unwrap()
    }
}
