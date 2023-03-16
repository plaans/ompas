use crate::aries::template::read_chronicle;
use crate::aries::useful;
use aries::core::Lit as aLit;
use aries::core::INT_CST_MAX;
use aries::model::lang::{Atom as aAtom, FAtom, SAtom};
use aries_planning::chronicles::{
    Chronicle, ChronicleInstance, ChronicleKind as aChronicleKind, ChronicleOrigin, Container, Ctx,
    Effect, SubTask, VarType, TIME_SCALE,
};
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::exec::state::INSTANCE;
use ompas_structs::acting_manager::planner_manager::BindingPlanner;
use ompas_structs::planning::instance::PlanningInstance;
use ompas_structs::planning::problem::PlanningProblem;
use ompas_structs::state::partial_state::PartialState;
use ompas_structs::state::world_state::{StateType, WorldStateSnapshot};
use sompas_structs::lvalues::LValueS;
use std::convert::TryInto;

pub fn create_initial_chronicle(problem: &PlanningProblem, ctx: &mut Ctx) -> ChronicleInstance {
    let instance = &problem.instance;
    let mut present_sf: Vec<String> = problem
        .domain
        .sf
        .iter()
        .map(|sf| sf.get_label().to_string())
        .collect();

    present_sf.append(&mut vec![QUANTITY.to_string(), MAX_Q.to_string()]);
    let _init_container = Container::Instance(0);
    // Initial chronicle construction
    let mut init_ch = Chronicle {
        kind: aChronicleKind::Problem,
        presence: aLit::TRUE,
        start: ctx.origin(),
        end: ctx.horizon(),
        name: vec![],
        task: None,
        conditions: vec![],
        effects: vec![],
        constraints: vec![],
        subtasks: vec![],
        cost: None,
    };

    initialize_state(&mut init_ch, &instance.state, &present_sf, ctx);
    initialize_goal_task(&mut init_ch, &instance.tasks, ctx);

    //println!("problem initialized");

    /*
    Goals: Add subtask
     */

    ChronicleInstance {
        parameters: vec![],
        origin: ChronicleOrigin::Original,
        chronicle: init_ch,
    }
}

/**
Add initial state from RAEStateSnapshot
 */
fn initialize_state(
    init_ch: &mut Chronicle,
    state: &WorldStateSnapshot,
    present_sf: &Vec<String>,
    ctx: &Ctx,
) {
    /*
    Initialisation of instance state variable
     */
    for (key, value) in &PartialState::from(state.instance.clone()).inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        assert_eq!(key.len(), 2);
        assert_eq!(&key[0].to_string(), INSTANCE);
        let sf: SAtom = useful::satom_from_lvalues(ctx, &key[0]);
        let t: SAtom = useful::satom_from_lvalues(ctx, &key[1]);
        let objects: Vec<LValueS> = value.try_into().expect("");
        for obj in &objects {
            let object: SAtom = useful::satom_from_lvalues(ctx, obj);
            let sv = vec![sf, object];
            init_ch.effects.push(Effect {
                transition_start: init_ch.start,
                persistence_start: init_ch.start,
                min_persistence_end: vec![],
                state_var: sv,
                value: t.into(),
            });
        }
    }

    /*
    Initialisation of static state variables
     */
    //We suppose for the moment that all args of state variable are objects
    'loop_static: for (key, value) in &state
        .get_state(Some(StateType::Static))
        .inner
        .union(state.get_state(Some(StateType::Dynamic)).inner)
    {
        let key: Vec<SAtom> = match key {
            LValueS::List(vec) => {
                let sf = vec[0].to_string();
                if present_sf.contains(&sf) {
                    vec.iter()
                        .map(|lv| useful::satom_from_lvalues(ctx, lv))
                        .collect()
                } else {
                    continue 'loop_static;
                }
            }
            LValueS::Symbol(sf) => {
                if present_sf.contains(sf) {
                    vec![useful::satom_from_lvalues(ctx, key)]
                } else {
                    continue 'loop_static;
                }
            }
            _ => panic!("state variable is either a symbol or a list of symbols"),
        };
        let value = useful::atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            min_persistence_end: vec![],
            state_var: key,
            value,
        });
    }
}

fn initialize_goal_task(init_ch: &mut Chronicle, goal_tasks: &[LValueS], ctx: &mut Ctx) {
    let c = Container::Instance(0);
    for t in goal_tasks {
        let t: Vec<LValueS> = t.try_into().expect("");
        let task_name: Vec<aAtom> = t
            .iter()
            .map(|v| useful::atom_from_lvalues(ctx, v))
            .collect();

        let prez = init_ch.presence;
        let start = ctx.model.new_optional_fvar(
            0,
            INT_CST_MAX,
            TIME_SCALE,
            prez,
            c / VarType::TaskStart(0),
        );
        let end =
            ctx.model
                .new_optional_fvar(0, INT_CST_MAX, TIME_SCALE, prez, c / VarType::TaskEnd(0));
        let start = FAtom::from(start);
        let end = FAtom::from(end);
        let id = None;
        let st = SubTask {
            id,
            start,
            end,
            task_name,
        };
        init_ch.subtasks.push(st);
    }
}

pub fn generate_instances(
    ctx: &mut Ctx,
    bindings: &mut BindingPlanner,
    instances: &mut Vec<ChronicleInstance>,
    instance: &PlanningInstance,
) -> anyhow::Result<()> {
    for (id, instance) in instance.instances.iter().enumerate() {
        let scope = match instance.origin {
            ChronicleOrigin::Refinement { instance_id, .. } => {
                instances[instance_id].chronicle.presence
            }
            _ => panic!(),
        };
        let template = read_chronicle(
            ctx,
            bindings,
            &instance.om.chronicle.as_ref().unwrap(),
            Container::Instance(id + 1),
            Some(scope),
        )?;
        //Printer::print_chronicle(&template.chronicle, &ctx.model);

        instances.push(ChronicleInstance {
            parameters: template
                .parameters
                .iter()
                .map(|v| aAtom::from(*v))
                .collect(),
            origin: instance.origin,
            chronicle: template.chronicle,
        });
    }
    Ok(())
}
