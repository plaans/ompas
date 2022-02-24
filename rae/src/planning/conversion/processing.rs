use crate::module::rae_exec::{
    RAE_ASSERT, RAE_ASSERT_SHORT, RAE_INSTANCE, RAE_RETRACT, RAE_RETRACT_SHORT,
};
use crate::planning::conversion::post_processing::post_processing;
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::chronicle::{Chronicle, ExpressionChronicle};
use crate::planning::structs::condition::Condition;
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::effect::Effect;
use crate::planning::structs::expression::Expression;
use crate::planning::structs::interval::Interval;
use crate::planning::structs::lit::{lvalue_to_lit, Lit};
use crate::planning::structs::symbol_table::ExpressionType;
use crate::planning::structs::traits::{Absorb, FormatWithSymTable, GetVariables};
use crate::planning::structs::transition::Transition;
use crate::planning::structs::{ChronicleHierarchy, ConversionContext, TaskType};
use ompas_lisp::core::structs::lcoreoperator::language::EVAL;
use ompas_lisp::core::structs::lcoreoperator::LCoreOperator;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_lisp::static_eval::{eval_static, parse_static};
use std::convert::TryInto;

//Names of the functions

//const PRE_PROCESSING: &str = "pre_processing";
const CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE: &str = "convert_lvalue_to_expression_chronicle";

pub const TRANSLATE_COND_IF: &str = "translate_cond_if";

pub fn convert_lvalue_to_expression_chronicle(
    exp: &LValue,
    context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<ExpressionChronicle, LError> {
    let mut ec = ExpressionChronicle::new(exp.clone(), &mut ch.sym_table);

    match exp {
        LValue::Symbol(s) => {
            //Generale case
            ec.set_pure_result(ch.sym_table.declare_new_symbol(s.into(), false).into());
        }
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_pure_result(lvalue_to_lit(exp, &mut ch.sym_table)?);
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    //Todo : handle the case when the first expression is not a symbol, but an expression that must be evaluated
                    if let LValue::Symbol(s) = &l[1] {
                        let var = ch.sym_table.declare_new_symbol(s.clone(), true);
                        let val = convert_lvalue_to_expression_chronicle(&l[2], context, ch)?;
                        if val.is_result_pure() {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_interval().start().into(),
                                ec.get_interval().end().into(),
                            ))
                        }
                        ec.add_constraint(Constraint::Eq(val.get_result(), var.into()));
                        ec.set_pure_result(ch.sym_table.new_bool(false).into());
                        ec.absorb(val);
                    } else {
                        return Err(SpecialError(
                            CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!(
                                "Define should take as first argument a symbol and {} is not.",
                                l[1]
                            ),
                        ));
                    }
                }
                LCoreOperator::Begin | LCoreOperator::Do => {
                    ch.sym_table.new_scope();
                    let mut literal: Vec<Lit> = vec![ch
                        .sym_table
                        .id(co.to_string().as_str())
                        .unwrap_or_else(|| panic!("{} is not defined in the symbol table", co))
                        .into()];
                    let mut previous_interval: Interval = *ec.get_interval();

                    for (i, e) in l[1..].iter().enumerate() {
                        let ec_i = convert_lvalue_to_expression_chronicle(e, context, ch)?;

                        literal.push(ec_i.get_result());
                        if i == 0 {
                            ec.add_constraint(Constraint::Eq(
                                previous_interval.start().into(),
                                ec_i.get_interval().start().into(),
                            ));
                        } else {
                            ec.add_constraint(Constraint::Eq(
                                previous_interval.end().into(),
                                ec_i.get_interval().start().into(),
                            ))
                        }

                        previous_interval = *ec_i.get_interval();

                        if i == l.len() - 2 {
                            if ec_i.is_result_pure() {
                                ec.set_pure_result(ec_i.get_result())
                            } else {
                                ec.add_effect(Effect {
                                    interval: *ec.get_interval(),
                                    transition: Transition::new(ec.get_result(), ec_i.get_result()),
                                });
                            }
                        }
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::Eq(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    ch.sym_table.revert_scope();
                }
                LCoreOperator::If => {
                    return convert_if(exp, context, ch);
                }
                LCoreOperator::Quote => {
                    ec.set_pure_result(lvalue_to_lit(&l[1], &mut ch.sym_table)?);
                }
                co => {
                    return Err(SpecialError(
                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                        format!("{} not supported yet\nexp : {}", co, exp),
                    ))
                }
            },
            _ => {
                let mut expression_type = ExpressionType::Lisp;

                ch.sym_table.new_scope();
                let mut literal: Vec<Lit> = vec![];

                match &l[0] {
                    LValue::Symbol(_) | LValue::Fn(_) => {
                        let s = l[0].to_string();
                        match s.as_str() {
                            RAE_ASSERT | RAE_ASSERT_SHORT => {
                                if l.len() != 3 {
                                    return Err(WrongNumberOfArgument(
                                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                        exp.clone(),
                                        l.len(),
                                        3..3,
                                    ));
                                }

                                let state_variable =
                                    convert_lvalue_to_expression_chronicle(&l[1], context, ch)?;
                                let value =
                                    convert_lvalue_to_expression_chronicle(&l[2], context, ch)?;

                                ec.add_effect(Effect {
                                    interval: *ec.get_interval(),
                                    transition: Transition::new(
                                        state_variable.get_result(),
                                        value.get_result(),
                                    ),
                                });

                                ec.absorb(state_variable);
                                ec.absorb(value);

                                ec.set_pure_result(ch.sym_table.new_bool(false).into());

                                return Ok(ec);
                            }
                            RAE_INSTANCE => {
                                expression_type = ExpressionType::StateFunction;
                                literal.push(
                                    ch.sym_table
                                        .id(RAE_INSTANCE)
                                        .unwrap_or_else(|| {
                                            panic!("{} is undefined in symbol table", RAE_INSTANCE)
                                        })
                                        .into(),
                                )
                            }
                            RAE_RETRACT | RAE_RETRACT_SHORT => {
                                return Err(SpecialError(
                                    CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                    "not yet supported".to_string(),
                                ))
                            }
                            _ => {
                                if let Some(id) = ch.sym_table.id(&s) {
                                    match ch.sym_table
                                        .get_type(id)
                                        .expect("a defined symbol should have a type")
                                    {
                                        AtomType::Action => {
                                            expression_type = ExpressionType::Action;
                                            println!("{} is an action", s);
                                        }
                                        AtomType::Function => {}
                                        AtomType::Method => return Err(SpecialError(CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{} is method and can not be directly called into the body of a method.\
                                \nPlease call the task that use the method instead", s))),
                                        AtomType::StateFunction => {
                                            expression_type = ExpressionType::StateFunction
                                        }
                                        AtomType::Task => {
                                            expression_type = ExpressionType::Task
                                        }
                                        _ => return Err(SpecialError(CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{}: first symbol should be a function, task, action or state function", s))),
                                    }
                                    literal.push(id.into())
                                } else {
                                    return Err(SpecialError(
                                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                        format!("function {} is not defined", s),
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(SpecialError(
                            CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!("{} is not yet supported", TypeLValue::from(&l[0])),
                        ))
                    }
                }

                let mut sub_expression_pure = true;

                let mut previous_interval = *ec.get_interval();
                for (i, e) in l[1..].iter().enumerate() {
                    let ec_i = convert_lvalue_to_expression_chronicle(e, context, ch)?;

                    literal.push(ec_i.get_result());
                    sub_expression_pure &= ec_i.is_result_pure();
                    if i != 0 {
                        ec.add_constraint(Constraint::Eq(
                            previous_interval.end().into(),
                            ec_i.get_interval().start().into(),
                        ));
                    } else {
                        ec.add_constraint(Constraint::Eq(
                            previous_interval.start().into(),
                            ec_i.get_interval().start().into(),
                        ));
                    }

                    previous_interval = *ec_i.get_interval();
                    ec.absorb(ec_i);
                }

                ec.add_constraint(Constraint::LEq(
                    previous_interval.end().into(),
                    ec.get_interval().end().into(),
                ));

                if sub_expression_pure && expression_type == ExpressionType::Lisp {
                    expression_type = ExpressionType::Pure;
                }

                match expression_type {
                    ExpressionType::Pure => {
                        let mut env = context.env.clone();
                        let mut is_pure = false;

                        let mut string = "(".to_string();
                        for (i, element) in literal.iter().enumerate() {
                            if i == 0 {
                                string.push_str(
                                    element.format_with_sym_table(&ch.sym_table).as_str(),
                                );
                                string.push(' ');
                            } else {
                                string.push_str(
                                    format!(
                                        "(quote {})",
                                        element.format_with_sym_table(&ch.sym_table)
                                    )
                                    .as_str(),
                                );
                            }
                        }
                        string.push(')');

                        println!("expression to be evaluated : {}", string);

                        let result = parse_static(string.as_str(), &mut env);
                        if let Ok(result) = result {
                            if result.is_pure() {
                                println!("parsing of result is pure");
                                let result = eval_static(result.get_lvalue(), &mut env);

                                match result {
                                    Ok(result) => {
                                        if result.is_pure() {
                                            ec.set_pure_result(lvalue_to_lit(
                                                result.get_lvalue(),
                                                &mut ch.sym_table,
                                            )?);
                                            is_pure = true;
                                            println!(
                                                "eval static is a success! result is: {}",
                                                result.get_lvalue()
                                            );
                                        } else {
                                            println!("result is not pure: ");
                                        }
                                    }
                                    Err(e) => {
                                        println!("Error in static evaluation: {}", e);
                                    }
                                }
                            }
                        } else {
                        }
                        if !is_pure {
                            let literal: Lit = vec![
                                ch.sym_table
                                    .id(EVAL)
                                    .expect("Eval not defined in symbol table")
                                    .into(),
                                Lit::from(literal),
                            ]
                            .into();

                            ec.add_effect(Effect {
                                interval: *ec.get_interval(),
                                transition: Transition::new(ec.get_result(), literal),
                            });
                        }
                    }
                    ExpressionType::Lisp => {
                        let literal: Lit = vec![
                            ch.sym_table
                                .id(EVAL)
                                .expect("Eval not defined in symbol table")
                                .into(),
                            Lit::from(literal),
                        ]
                        .into();

                        ec.add_effect(Effect {
                            interval: *ec.get_interval(),
                            transition: Transition::new(ec.get_result(), literal),
                        });
                    }
                    ExpressionType::Action | ExpressionType::Task => {
                        literal.push(ec.get_result());
                        ec.add_subtask(Expression {
                            interval: *ec.get_interval(),
                            lit: literal.into(),
                        })
                    }
                    ExpressionType::StateFunction => {
                        ec.add_effect(Effect {
                            interval: *ec.get_interval(),
                            transition: Transition::new(ec.get_result(), literal.into()),
                        });
                    }
                };
                ch.sym_table.revert_scope();
            }
        },
        lv => {
            return Err(SpecialError(
                CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                format!(
                    "{} not supported yet\n exp: {}",
                    TypeLValue::from(lv),
                    exp.format(" exp: ".len())
                ),
            ))
        }
    }

    Ok(ec)
}

pub fn convert_if(
    exp: &LValue,
    context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<ExpressionChronicle, LError> {
    let mut ec: ExpressionChronicle = ExpressionChronicle::new(exp.clone(), &mut ch.sym_table);

    let exp: Vec<LValue> = exp
        .try_into()
        .expect("could not transform expression into list");

    let cond = &exp[1];
    let b_true = &exp[2];
    let b_false = &exp[3];

    let ec_cond = convert_lvalue_to_expression_chronicle(cond, context, ch)?;
    let mut ec_b_true = convert_lvalue_to_expression_chronicle(b_true, context, ch)?;
    let mut ec_b_false = convert_lvalue_to_expression_chronicle(b_false, context, ch)?;

    let cond_var = ec_cond.get_result();

    let variables = ec_cond
        .get_variables()
        .union(ec_b_true.get_variables().union(ec_b_false.get_variables()));

    let task_label = ch.local_tasks.new_label(TaskType::IfTask);
    let task_symbol_id = ch.sym_table.declare_new_symbol(task_label.clone(), true);

    let mut variables_lit: Vec<Lit> = variables.iter().map(|var| Lit::from(*var)).collect();

    let mut task: Vec<Lit> = vec![task_symbol_id.into(), cond_var.clone()];
    task.append(&mut variables_lit.clone());

    //Construction of the method for the branch true
    let method_true_label = format!("m_{}_true", task_label);
    let method_true_label = ch.sym_table.declare_new_symbol(method_true_label, true);
    let mut method_true_name: Vec<Lit> = vec![method_true_label.into(), cond_var.clone()];
    method_true_name.append(&mut variables_lit.clone());
    let mut method_true = Chronicle::default();
    method_true.set_debug(Some(b_true.clone()));
    method_true.set_task(task.clone().into());
    method_true.set_name(method_true_name.into());
    ec_b_true.add_condition(Condition {
        interval: Interval::new(
            &ec_b_true.get_interval().start(),
            &ec_b_true.get_interval().start(),
        ),
        constraint: Constraint::Eq(cond_var.clone(), ch.sym_table.new_bool(true).into()),
    });
    ec_b_true.add_variables(variables.clone());
    post_processing(&mut ec_b_true, context, ch)?;
    method_true.absorb_expression_chronicle(ec_b_true);

    //Construction of the method for the branch false
    let method_false_label = format!("m_{}_false", task_label);
    let method_false_label = ch.sym_table.declare_new_symbol(method_false_label, true);
    let mut method_false_name: Vec<Lit> = vec![method_false_label.into(), cond_var.clone()];
    method_false_name.append(&mut variables_lit);
    let mut method_false = Chronicle::default();
    method_false.set_debug(Some(b_false.clone()));
    method_false.set_task(task.clone().into());
    method_false.set_name(method_false_name.into());
    ec_b_false.add_condition(Condition {
        interval: Interval::new(
            &ec_b_false.get_interval().start(),
            &ec_b_false.get_interval().start(),
        ),
        constraint: Constraint::Eq(cond_var, ch.sym_table.new_bool(false).into()),
    });
    ec_b_false.add_variables(variables);
    post_processing(&mut ec_b_false, context, ch)?;

    method_false.absorb_expression_chronicle(ec_b_false);

    ch.tasks.push(task.clone().into());
    ch.methods.push(method_true);
    ch.methods.push(method_false);

    //Construction of the top chronicle
    let sub_task_interval = ch.sym_table.declare_new_interval();

    ec.add_constraint(Constraint::Eq(
        ec.get_interval().start().into(),
        ec_cond.get_interval().start().into(),
    ));

    ec.add_constraint(Constraint::Eq(
        ec_cond.get_interval().end().into(),
        sub_task_interval.start().into(),
    ));

    ec.add_constraint(Constraint::Eq(
        ec.get_interval().end().into(),
        sub_task_interval.end().into(),
    ));

    ec.absorb(ec_cond);
    ec.add_subtask(Expression {
        interval: sub_task_interval,
        lit: task.into(),
    });

    Ok(ec)

    //Construction of the first method;
}
