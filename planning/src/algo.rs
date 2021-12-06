use crate::structs::Lit;
use crate::structs::*;
use ompas_lisp::language::scheme_primitives::EVAL;
use ompas_lisp::structs::{LCoreOperator, LValue};

pub fn translate_domain_env_to_hierarchy(context: Context) -> (Domain, SymTable) {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let mut symbol_table = SymTable::default();

    //Add actions, tasks and methods symbols to symbol_table:
    for (label, _type) in context.domain.get_map_symbol_type().iter() {
        symbol_table.declare_new_object(Some(label.clone()), false);
    }

    let mut methods = vec![];
    #[allow(unused_mut)]
    let mut tasks = vec![];
    #[allow(unused_mut)]
    let mut actions = vec![];

    for (action_label, action) in context.domain.get_actions() {
        //evaluate the lambda sim.
        let mut chronicle = Chronicle::default();
        let symbol_id = symbol_table.declare_new_object(Some(action_label.clone()), false);
        let mut name = vec![symbol_id];

        for e in action.get_parameters().get_params() {
            let symbol_id = symbol_table.declare_new_object(Some(e), true);
            name.push(symbol_id);
            chronicle.add_var(&symbol_id);
        }

        let ec = translate_lvalue_to_expression_chronicle_r_2(
            action.get_sim(),
            &context,
            &mut symbol_table,
        );

        chronicle.absorb_expression_chronicle(ec);

        chronicle.set_name(name);

        actions.push(chronicle)
    }

    //Add all methods to the domain
    for (method_label, method) in context.domain.get_methods() {
        let mut chronicle = Chronicle::default();

        //Declaring the method
        {
            let symbol_id = symbol_table.declare_new_object(Some(method_label.clone()), false);
            let mut name = vec![symbol_id];

            for e in method.get_parameters().get_params() {
                let symbol_id = symbol_table.declare_new_object(Some(e), true);
                name.push(symbol_id);
                chronicle.add_var(&symbol_id)
            }

            chronicle.set_name(name);
        }

        //declaring symbols of the task
        {
            let task_label = method.get_task_label();
            let symbol_id = symbol_table.declare_new_object(Some(task_label.clone()), false);
            let mut task_name = vec![symbol_id];
            let task = context.domain.get_tasks().get(task_label).unwrap();

            for e in task.get_parameters().get_params() {
                let symbol_id = symbol_table.declare_new_object(Some(e), false);
                task_name.push(symbol_id);
                chronicle.add_var(&symbol_id)
            }

            chronicle.set_task(task_name);
        }

        methods.push(chronicle);
    }

    (Domain::new(actions, tasks, methods), symbol_table)
}

pub fn translate_lvalue_to_expression_chronicle_r_2(
    exp: &LValue,
    context: &Context,
    symbol_table: &mut SymTable,
) -> ExpressionChronicle {
    let mut ec = ExpressionChronicle::new(exp.clone(), symbol_table);

    match exp {
        LValue::Symbol(_) => {
            ec.set_lit(Lit::LValue(vec![EVAL.into(), exp.clone()].into()));
            ec.add_effect(Effect {
                interval: TransitionInterval::new(
                    *ec.get_interval(),
                    symbol_table.declare_new_timepoint(),
                ),
                transition: Transition::new(ec.get_result().into(), ec.get_lit()),
            });
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    let var = symbol_table.declare_new_object(Some(l[1].to_string()), true);
                    let val =
                        translate_lvalue_to_expression_chronicle_r_2(&l[2], context, symbol_table);
                    ec.add_constraint(Constraint::Eq(val.get_result().into(), var.into()));
                    ec.add_constraint(Constraint::Eq(ec.get_result().into(), LValue::Nil.into()));
                    ec.absorb(val);
                }
                LCoreOperator::Begin => {
                    symbol_table.new_scope();
                    let mut literal: Vec<Lit> = vec!["begin".into()];
                    let mut previous_interval = *ec.get_interval();
                    for (i, e) in l[1..].iter().enumerate() {
                        let mut ec_i =
                            translate_lvalue_to_expression_chronicle_r_2(e, context, symbol_table);

                        literal.push(ec_i.get_result().into());

                        ec_i.add_constraint(Constraint::LT(
                            previous_interval.end().into(),
                            ec_i.get_interval().start().into(),
                        ));

                        previous_interval = *ec_i.get_interval();

                        if i == l.len() - 2 {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_i.get_result().into(),
                            ))
                        }
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    let literal: Lit = vec![EVAL.into(), literal.into()].into();

                    /*let expression_chronicle = Expression {
                        interval: ec.get_interval().clone(),
                        lit: literal.clone(),
                    };*/

                    ec.set_lit(literal);
                    let persistence = symbol_table.declare_new_timepoint();

                    ec.add_var(&persistence);
                    ec.add_effect(Effect {
                        interval: TransitionInterval::new(*ec.get_interval(), persistence),
                        transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                    });
                    //ec.add_subtask(expression_chronicle);
                    symbol_table.revert_scope();
                }
                LCoreOperator::If => {
                    let cond = &l[1];
                    let a = &l[2];
                    let b = &l[3];

                    //Taking in account 1 branch if
                    if b == &LValue::Nil {
                        ec.add_condition(Condition {
                            interval: Interval::new(
                                &ec.get_interval().start(),
                                &ec.get_interval().start(),
                            ),
                            constraint: Constraint::Eq(cond.into(), "true".into()),
                        });

                        let ec_a =
                            translate_lvalue_to_expression_chronicle_r_2(a, context, symbol_table);

                        ec.add_constraint(Constraint::Eq(
                            ec.get_result().into(),
                            ec_a.get_result().into(),
                        ));
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().start().into(),
                            ec_a.get_interval().start().into(),
                        ));
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().end().into(),
                            ec_a.get_interval().end().into(),
                        ));
                    } else if a == &LValue::Nil {
                        ec.add_condition(Condition {
                            interval: Interval::new(
                                &ec.get_interval().start(),
                                &ec.get_interval().start(),
                            ),
                            constraint: Constraint::Eq(cond.into(), "false".into()),
                        });

                        let ec_b =
                            translate_lvalue_to_expression_chronicle_r_2(b, context, symbol_table);

                        ec.add_constraint(Constraint::Eq(
                            ec.get_result().into(),
                            ec_b.get_result().into(),
                        ));
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().start().into(),
                            ec_b.get_interval().start().into(),
                        ));
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().end().into(),
                            ec_b.get_interval().end().into(),
                        ));
                    }
                    //Two blocks where we have to define two methods
                    else {
                        panic!("not supported yet")
                    }
                }
                _ => panic!("not supported yet"),
            },
            _ => {
                symbol_table.new_scope();
                let mut literal: Vec<Lit> = vec![];
                let mut previous_interval = *ec.get_interval();
                for e in l {
                    let mut ec_i =
                        translate_lvalue_to_expression_chronicle_r_2(e, context, symbol_table);

                    literal.push(ec_i.get_result().into());

                    ec_i.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec_i.get_interval().start().into(),
                    ));

                    previous_interval = *ec_i.get_interval();
                    ec.absorb(ec_i);
                }

                ec.add_constraint(Constraint::LT(
                    previous_interval.end().into(),
                    ec.get_interval().end().into(),
                ));

                let literal: Lit = vec![EVAL.into(), literal.into()].into();

                ec.set_lit(literal);

                let persistence = symbol_table.declare_new_timepoint();
                ec.add_var(&persistence);

                ec.add_effect(Effect {
                    interval: TransitionInterval::new(*ec.get_interval(), persistence),
                    transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                });
                //ec.add_subtask(expression_chronicle);
                symbol_table.revert_scope();
            }
        },
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_lit(exp.into());
            ec.add_constraint(Constraint::Eq(ec.get_result().into(), ec.get_lit()))
        }
        _ => {
            panic!("not supported yet")
        }
    }

    ec
}

pub fn translate_lvalue_to_expression_chronicle_r(
    exp: &LValue,
    sym_table: &mut SymTable,
) -> ExpressionChronicle {
    let mut ec = ExpressionChronicle::new(exp.clone(), sym_table);

    match exp {
        LValue::Symbol(_) => {
            ec.set_lit(Lit::LValue(vec![EVAL.into(), exp.clone()].into()));
            ec.add_effect(Effect {
                interval: TransitionInterval::new(
                    *ec.get_interval(),
                    sym_table.declare_new_timepoint(),
                ),
                transition: Transition::new(ec.get_result().into(), ec.get_lit()),
            });
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    let var = sym_table.declare_new_object(Some(l[1].to_string()), true);
                    let val = translate_lvalue_to_expression_chronicle_r(&l[2], sym_table);
                    ec.add_constraint(Constraint::Eq(val.get_result().into(), var.into()));
                    ec.add_constraint(Constraint::Eq(ec.get_result().into(), LValue::Nil.into()));
                    ec.absorb(val);
                }
                LCoreOperator::Begin => {
                    sym_table.new_scope();
                    let mut literal: Vec<Lit> = vec!["begin".into()];
                    let mut previous_interval = *ec.get_interval();
                    for (i, e) in l[1..].iter().enumerate() {
                        let mut ec_i = translate_lvalue_to_expression_chronicle_r(e, sym_table);

                        literal.push(ec_i.get_result().into());

                        ec_i.add_constraint(Constraint::LT(
                            previous_interval.end().into(),
                            ec_i.get_interval().start().into(),
                        ));

                        previous_interval = *ec_i.get_interval();

                        if i == l.len() - 2 {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_i.get_result().into(),
                            ))
                        }
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    let literal: Lit = vec![EVAL.into(), literal.into()].into();

                    /*let expression_chronicle = Expression {
                        interval: ec.get_interval().clone(),
                        lit: literal.clone(),
                    };*/

                    ec.set_lit(literal);
                    let persistence = sym_table.declare_new_timepoint();

                    ec.add_var(&persistence);
                    ec.add_effect(Effect {
                        interval: TransitionInterval::new(*ec.get_interval(), persistence),
                        transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                    });
                    //ec.add_subtask(expression_chronicle);
                    sym_table.revert_scope();
                }
                _ => panic!("not supported yet"),
            },
            _ => {
                sym_table.new_scope();
                let mut literal: Vec<Lit> = vec![];
                let mut previous_interval = *ec.get_interval();
                for e in l {
                    let mut ec_i = translate_lvalue_to_expression_chronicle_r(e, sym_table);

                    literal.push(ec_i.get_result().into());

                    ec_i.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec_i.get_interval().start().into(),
                    ));

                    previous_interval = *ec_i.get_interval();
                    ec.absorb(ec_i);
                }

                ec.add_constraint(Constraint::LT(
                    previous_interval.end().into(),
                    ec.get_interval().end().into(),
                ));

                let literal: Lit = vec![EVAL.into(), literal.into()].into();

                ec.set_lit(literal);

                let persistence = sym_table.declare_new_timepoint();
                ec.add_var(&persistence);

                ec.add_effect(Effect {
                    interval: TransitionInterval::new(*ec.get_interval(), persistence),
                    transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                });
                //ec.add_subtask(expression_chronicle);
                sym_table.revert_scope();
            }
        },
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_lit(exp.into());
            ec.add_constraint(Constraint::Eq(ec.get_result().into(), ec.get_lit()))
        }
        _ => {
            panic!("not supported yet")
        }
    }

    ec
}

pub fn translate_lvalue_to_chronicle(exp: &[LValue], sym_table: &mut SymTable) -> Chronicle {
    let mut chronicle = Chronicle::default();
    let interval = sym_table.declare_new_interval();
    chronicle.add_interval(&interval);
    let result = sym_table.declare_new_result();
    chronicle.add_var(&result);
    let f = sym_table.declare_new_object(Some(exp[0].to_string()), false);
    let mut literal: Vec<Lit> = vec![f.into()];

    let mut previous_interval = interval;
    for e in &exp[1..] {
        let expression = Expression {
            interval: sym_table.declare_new_interval(),
            lit: Lit::LValue(e.clone()),
        };

        let result_e = sym_table.declare_new_result();
        literal.push(result_e.into());
        chronicle.add_interval(&expression.interval);
        chronicle.add_var(&result_e);

        let persistence = sym_table.declare_new_timepoint();
        chronicle.add_var(&persistence);
        let effect = Effect {
            interval: TransitionInterval::new(expression.interval, persistence),
            transition: Transition::new(
                result_e.into(),
                LValue::from(vec!["eval".into(), e.clone()]).into(),
            ),
        };

        chronicle.add_constraint(Constraint::LT(
            previous_interval.end().into(),
            expression.interval.start().into(),
        ));

        previous_interval = expression.interval;

        chronicle.add_effect(effect);
        chronicle.add_subtask(expression);
    }

    chronicle.add_constraint(Constraint::LT(
        previous_interval.end().into(),
        interval.end().into(),
    ));

    let expression_chronicle = Expression {
        interval,
        lit: Lit::from(literal.clone()),
    };

    chronicle.add_effect(Effect {
        interval: TransitionInterval::new(interval, sym_table.declare_new_timepoint()),
        transition: Transition::new(
            result.into(),
            LValue::from(vec![
                LValue::from("eval"),
                expression_chronicle
                    .lit
                    .format_with_sym_table(sym_table)
                    .into(),
            ])
            .into(),
        ),
    });
    chronicle.add_subtask(expression_chronicle);
    chronicle
}
