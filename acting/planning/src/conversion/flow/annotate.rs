use ompas_language::exec::acting_context::{
    CTX_ACQUIRE, CTX_ARBITRARY, CTX_EXEC_COMMAND, CTX_EXEC_TASK,
};
use ompas_language::exec::platform::EXEC_COMMAND;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::exec::resource::ACQUIRE;
use ompas_language::exec::ARBITRARY;
use sompas_language::kind::LIST;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;

#[derive(Clone)]
pub enum AnnotateFrame {
    Lv(LValue),
    List(usize),
}

#[derive(Default)]
pub struct AnnotateStack {
    frames: Vec<AnnotateFrame>,
    results: Vec<LValue>,
}

pub fn annotate(lv: LValue) -> LValue {
    let mut n_acquire = -1;
    let mut n_command = -1;
    let mut n_arbitrary = -1;
    let mut n_exec_task = -1;

    let mut stack = AnnotateStack::default();
    stack.frames.push(AnnotateFrame::Lv(lv));

    let ctx_expr = |ctx: &str, id: i32, mut args: Vec<LValue>| -> LValue {
        args.insert(0, LIST.into());
        list![ctx.into(), id.into(), args.into()]
    };

    while let Some(frame) = stack.frames.pop() {
        match frame {
            AnnotateFrame::Lv(lv) => {
                if let LValue::List(list) = lv {
                    stack.frames.push(AnnotateFrame::List(list.len()));
                    list.iter()
                        .rev()
                        .for_each(|lv| stack.frames.push(AnnotateFrame::Lv(lv.clone())))
                } else {
                    stack.results.push(lv)
                }
            }
            AnnotateFrame::List(n) => {
                let mut results = stack.results.split_off(stack.results.len() - n);
                let result = match results[0].to_string().as_str() {
                    EXEC_COMMAND => {
                        n_command += 1;
                        ctx_expr(CTX_EXEC_COMMAND, n_command, results.split_off(1))
                    }
                    ACQUIRE => {
                        n_acquire += 1;
                        ctx_expr(CTX_ACQUIRE, n_acquire, results.split_off(1))
                    }
                    ARBITRARY => {
                        n_arbitrary += 1;
                        ctx_expr(CTX_ARBITRARY, n_arbitrary, results.split_off(1))
                    }
                    EXEC_TASK => {
                        n_exec_task += 1;
                        ctx_expr(CTX_EXEC_TASK, n_exec_task, results.split_off(1))
                    }
                    _ => results.into(),
                };
                stack.results.push(result);
            }
        }
    }

    stack.results.remove(0)
}
