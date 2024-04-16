use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::model::ModelKind::PlanModel;
use crate::model::acting_domain::OMPASDomain;
use crate::model::chronicle::ChronicleKind;
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::state::ModState;
use crate::planning::planner::encoding::problem_generation::{generate_acting_model, ActionParam};
use im::HashMap;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lvalue::LValue;
use std::fmt::{Debug, Formatter};

#[derive(Default, Clone)]
pub struct ActingModelCollection {
    pub tasks: HashMap<String, ActingModel>,
    pub methods: HashMap<String, ActingModel>,
    pub commands: HashMap<String, ActingModel>,
}

impl Debug for ActingModelCollection {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl ActingModelCollection {
    pub async fn pre_compute_acting_models(
        &mut self,
        domain: &OMPASDomain,
        env: &LEnv,
        state: WorldStateSnapshot,
        st: &RefSymTable,
    ) {
        let mut env = env.clone();
        env.update_context(ModState::new_from_snapshot(state));
        //Add tasks to domain

        //println!("Start task declaration.");
        for (label, task) in domain.tasks.iter() {
            let mut action_task: Vec<ActionParam> =
                vec![ActionParam::Instantiated(label.to_string().into())];
            task.get_parameters()
                .get_labels()
                .drain(..)
                .for_each(|p| action_task.push(ActionParam::Uninstantiated(p.to_string().into())));

            if let Some(LValue::Lambda(lambda)) = task.get_model(&PlanModel) {
                //evaluate the lambda sim.
                //println!("Converting command {}", command.get_label());
                if let Ok(am) = generate_acting_model(
                    &lambda,
                    &action_task,
                    None,
                    task.get_parameters(),
                    st,
                    &env,
                    ChronicleKind::Task,
                )
                .await
                {
                    if am.chronicle.is_some() {
                        self.tasks.insert(label.to_string(), am);
                    }
                }
            } else {
                for label in task.get_methods() {
                    let method = domain.methods.get(label).unwrap();

                    let mut action_method: Vec<ActionParam> =
                        vec![ActionParam::Instantiated(label.to_string().into())];
                    method
                        .get_parameters()
                        .get_labels()
                        .drain(..)
                        .for_each(|p| {
                            action_method.push(ActionParam::Uninstantiated(p.to_string().into()))
                        });

                    let method_lambda: LLambda = method
                        .get_body()
                        .try_into()
                        .expect("The body of a method should be lambda.");

                    if let Ok(am) = generate_acting_model(
                        &method_lambda,
                        &action_method,
                        Some(&action_task),
                        method.get_parameters(),
                        st,
                        &env,
                        ChronicleKind::Method,
                    )
                    .await
                    {
                        if am.chronicle.is_some() {
                            self.methods.insert(label.to_string(), am);
                        }
                    }
                }
            }
        }
        //println!("End task declaration.");

        //println!("Start command declaration.");
        for (label, command) in domain.get_commands() {
            let mut action_command: Vec<ActionParam> =
                vec![ActionParam::Instantiated(label.to_string().into())];
            command
                .get_parameters()
                .get_labels()
                .drain(..)
                .for_each(|p| {
                    action_command.push(ActionParam::Uninstantiated(p.to_string().into()))
                });

            if let Some(LValue::Lambda(lambda)) = command.get_model(&PlanModel) {
                //evaluate the lambda sim.
                //println!("Converting command {}", command.get_label());
                if let Ok(am) = generate_acting_model(
                    &lambda,
                    &action_command,
                    None,
                    command.get_parameters(),
                    st,
                    &env,
                    ChronicleKind::Command,
                )
                .await
                {
                    if am.chronicle.is_some() {
                        self.commands.insert(label.to_string(), am);
                    }
                }
            }
        }
    }

    pub fn get_task_model(&self, task: &str) -> Option<&ActingModel> {
        self.tasks.get(task)
    }

    pub fn get_method_model(&self, method: &str) -> Option<&ActingModel> {
        self.methods.get(method)
    }

    pub fn get_command_model(&self, command: &str) -> Option<&ActingModel> {
        self.commands.get(command)
    }
}

impl ActingModelCollection {
    pub fn try_instantiate_task(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.get_task_model(&args[0].as_ref().unwrap().to_string())
            .map(|am| am.duplicate_with_args(args, st))
    }

    pub fn try_instantiate_command(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.get_command_model(&args[0].as_ref().unwrap().to_string())
            .map(|am| am.duplicate_with_args(args, st))
    }

    pub fn try_instantiate_method(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.get_method_model(&args[0].as_ref().unwrap().to_string())
            .map(|am| am.duplicate_with_args(args, st))
    }
}
