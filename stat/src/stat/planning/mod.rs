use crate::stat::Stat;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PlanningField {
    PlanningTimes,
    PlanningSolutions,
    PlanningSuccesses,
}

impl PlanningField {
    pub fn to_latex(&self) -> String {
        format!("${}$", self)
    }
}

impl Display for PlanningField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PlanningField::PlanningTimes => {
                    "PT"
                }
                PlanningField::PlanningSolutions => {
                    "PS"
                }
                PlanningField::PlanningSuccesses => {
                    "PSR"
                }
            }
        )
    }
}

pub struct ConfigPlanningStat {
    pub stat_map: HashMap<PlanningField, Vec<Stat>>,
}

impl ConfigPlanningStat {
    pub fn get(&self, field: &PlanningField) -> Option<&Vec<Stat>> {
        self.stat_map.get(field)
    }

    pub fn get_mut(&mut self, field: &PlanningField) -> Option<&mut Vec<Stat>> {
        self.stat_map.get_mut(field)
    }
}
