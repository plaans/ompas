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
    pub fn unit(&self) -> String {
        match self {
            PlanningField::PlanningTimes => "seconds",
            PlanningField::PlanningSolutions => "",
            PlanningField::PlanningSuccesses => "percentage",
        }
        .to_string()
    }
}

impl Display for PlanningField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PlanningField::PlanningTimes => {
                    "T_P"
                }
                PlanningField::PlanningSolutions => {
                    "S_P"
                }
                PlanningField::PlanningSuccesses => {
                    "Cov_P"
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
