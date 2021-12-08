use crate::structs::Lit;
use crate::structs::*;
use ompas_lisp::core::{eval, expand, parse, ContextCollection, LEnv};
use ompas_lisp::language::scheme_primitives::EVAL;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{LCoreOperator, LError, LResult, LValue, LambdaArgs, NameTypeLValue};
use ompas_utils::blocking_async;

//Names of the functions

const PRE_PROCESSING: &str = "pre_processing";
const TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE_R_2: &str =
    "translate_lvalue_to_expression_chronicle_r_2";

pub fn pre_processing(lv: &LValue, context: &Context) -> LResult {
    let mut lv = match transform_lambda_expression(lv, context.env.clone(), context.ctxs.clone()) {
        Ok(lv) => lv,
        Err(_) => lv.clone(),
    };

    if let LValue::List(list) = &lv {
        let mut result = vec![];
        for lv in list {
            result.push(pre_processing(lv, context)?)
        }

        lv = result.into()
    }

    Ok(lv)
}

pub fn translate_domain_env_to_hierarchy(context: Context) -> Result<(Domain, SymTable), LError> {
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
        )?;

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

    Ok((Domain::new(actions, tasks, methods), symbol_table))
}

pub fn translate_lvalue_to_expression_chronicle_r_2(
    exp: &LValue,
    context: &Context,
    symbol_table: &mut SymTable,
) -> Result<ExpressionChronicle, LError> {
    let mut ec = ExpressionChronicle::new(exp.clone(), symbol_table);

    match exp {
        LValue::Symbol(s) => {
            ec.set_lit(
                vec![
                    Lit::from(
                        symbol_table
                            .id(&EVAL.into())
                            .expect("eval should be in the sym table"),
                    ),
                    symbol_table
                        .declare_new_object(Some(s.into()), false)
                        .into(),
                ]
                .into(),
            );
            ec.add_effect(Effect {
                interval: *ec.get_interval(),
                transition: Transition::new(ec.get_result().into(), ec.get_lit()),
            });
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    let var = symbol_table.declare_new_object(Some(l[1].to_string()), true);
                    let val =
                        translate_lvalue_to_expression_chronicle_r_2(&l[2], context, symbol_table)?;
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
                            translate_lvalue_to_expression_chronicle_r_2(e, context, symbol_table)?;

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

                    let literal: Lit = literal.into();

                    let literal: Lit = vec![EVAL.into(), literal].into();

                    /*let expression_chronicle = Expression {
                        interval: ec.get_interval().clone(),
                        lit: literal.clone(),
                    };*/

                    ec.set_lit(literal);

                    ec.add_effect(Effect {
                        interval: *ec.get_interval(),
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
                            translate_lvalue_to_expression_chronicle_r_2(a, context, symbol_table)?;

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
                        ec.absorb(ec_a);
                    } else if a == &LValue::Nil {
                        ec.add_condition(Condition {
                            interval: Interval::new(
                                &ec.get_interval().start(),
                                &ec.get_interval().start(),
                            ),
                            constraint: Constraint::Eq(cond.into(), "false".into()),
                        });

                        let ec_b =
                            translate_lvalue_to_expression_chronicle_r_2(b, context, symbol_table)?;

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
                        ec.absorb(ec_b);
                    }
                    //Two blocks where we have to define two methods
                    else {
                        return Err(SpecialError(
                            TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE_R_2,
                            "Two branches If block is not supported yet...".to_string(),
                        ));
                    }
                }
                LCoreOperator::Quote => {
                    ec =
                        translate_lvalue_to_expression_chronicle_r_2(&l[1], context, symbol_table)?;
                }
                co => {
                    return Err(SpecialError(
                        TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE_R_2,
                        format!("{} not supported yet", co),
                    ))
                }
            },
            _ => {
                symbol_table.new_scope();
                let mut literal: Vec<Lit> = vec![];
                let mut previous_interval = *ec.get_interval();
                for e in l {
                    let mut ec_i =
                        translate_lvalue_to_expression_chronicle_r_2(e, context, symbol_table)?;

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

                let literal: Lit = vec![EVAL.into(), Lit::from(literal)].into();

                ec.set_lit(literal);

                ec.add_effect(Effect {
                    interval: *ec.get_interval(),
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

    Ok(ec)
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

        let effect = Effect {
            interval: expression.interval,
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
        interval,
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

pub const TRANSFROM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub fn transform_lambda_expression(lv: &LValue, env: LEnv, ctxs: ContextCollection) -> LResult {
    //println!("in transform lambda");

    if let LValue::List(list) = lv {
        if list.is_empty() {
            return Err(WrongNumberOfArgument(
                TRANSFROM_LAMBDA_EXPRESSION,
                lv.clone(),
                0,
                1..std::usize::MAX,
            ));
        }

        let arg = list[0].clone();
        let mut c_env = env.clone();
        let mut c_ctxs = ctxs.clone();

        let lambda = blocking_async!({
            eval(
                &expand(&arg, true, &mut c_env, &mut c_ctxs).await?,
                &mut c_env,
                &mut c_ctxs,
            )
            .await
        })
        .expect("Error in thread evaluating lambda")?;
        //println!("evaluating is a success");
        if let LValue::Lambda(l) = lambda {
            let mut lisp = "(begin".to_string();

            let args = &list[1..];

            let params = l.get_params();
            let body = l.get_body();

            match params {
                LambdaArgs::Sym(param) => {
                    let arg = if args.len() == 1 {
                        match &args[0] {
                            LValue::Nil => LValue::Nil,
                            _ => vec![args[0].clone()].into(),
                        }
                    } else {
                        args.into()
                    };
                    lisp.push_str(format!("(define {} '{})", param, arg).as_str());
                }
                LambdaArgs::List(params) => {
                    if params.len() != args.len() {
                        return Err(SpecialError(
                            TRANSFROM_LAMBDA_EXPRESSION,
                            format!(
                                "in lambda {}: ",
                                WrongNumberOfArgument(
                                    TRANSFROM_LAMBDA_EXPRESSION,
                                    args.into(),
                                    args.len(),
                                    params.len()..params.len(),
                                )
                            ),
                        ));
                    }
                    for (param, arg) in params.iter().zip(args) {
                        lisp.push_str(format!("(define {} '{})", param.to_string(), arg).as_str());
                    }
                }
                LambdaArgs::Nil => {
                    if !args.is_empty() {
                        return Err(SpecialError(
                            TRANSFROM_LAMBDA_EXPRESSION,
                            "Lambda was expecting no args.".to_string(),
                        ));
                    }
                }
            };

            lisp.push_str(body.to_string().as_str());
            lisp.push(')');

            let mut c_env = env;
            let mut c_ctxs = ctxs;

            blocking_async!(parse(&lisp, &mut c_env, &mut c_ctxs).await)
                .expect("error in thread parsing string")
        } else {
            Err(WrongType(
                TRANSFROM_LAMBDA_EXPRESSION,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Lambda,
            ))
        }
    } else {
        Err(WrongType(
            TRANSFROM_LAMBDA_EXPRESSION,
            lv.clone(),
            lv.into(),
            NameTypeLValue::List,
        ))
    }
}
