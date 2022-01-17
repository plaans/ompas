use crate::structs::Constraint;
use crate::structs::Lit;
use crate::structs::*;
use ompas_acting::rae::module::rae_exec::{RAE_ASSERT, RAE_RETRACT};
use ompas_lisp::core::{eval, expand, parse, ContextCollection, LEnv};
use ompas_lisp::language::scheme_primitives::*;
use ompas_lisp::structs::LError::{
    NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use ompas_lisp::structs::{LCoreOperator, LError, LResult, LValue, LambdaArgs, NameTypeLValue};
use ompas_utils::blocking_async;

//Names of the functions

//const PRE_PROCESSING: &str = "pre_processing";
const TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE: &str = "translate_lvalue_to_expression_chronicle";

const TRANSLATE_LVALUE_TO_CHRONICLE: &str = "translate_lvalue_to_chronicle";

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

pub fn unify_equal(ec: &mut ExpressionChronicle, sym_table: &mut SymTable, _context: &Context) {
    let mut vec_constraint_to_rm = vec![];

    for (index, constraint) in ec.get_constraints().iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                let type_1 = sym_table.get_type(id_1).expect("id should be defined");
                let type_2 = sym_table.get_type(id_2).expect("id should be defined");
                match (type_1, type_2) {
                    (
                        AtomType::Boolean | AtomType::Number,
                        AtomType::Boolean | AtomType::Number,
                    ) => {
                        assert_eq!(
                            sym_table.get_atom(id_1).unwrap(),
                            sym_table.get_atom(id_2).unwrap()
                        );
                    }
                    (AtomType::Number, AtomType::Timepoint) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Timepoint, AtomType::Number) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Boolean | AtomType::Number, _) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (_, AtomType::Boolean | AtomType::Number) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Symbol, AtomType::Object | AtomType::Result) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Object | AtomType::Result, AtomType::Symbol) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Result, AtomType::Result) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Timepoint, AtomType::Timepoint) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (_, _) => {}
                }
            }
        }
    }

    ec.rm_set_constraint(vec_constraint_to_rm)
}

pub fn rm_useless_var(ec: &mut ExpressionChronicle, sym_table: &mut SymTable, _context: &Context) {
    let mut vec = vec![];

    for var in ec.get_variables() {
        if *var != sym_table.get_parent(var) {
            vec.push(*var)
        }
    }

    ec.rm_set_var(vec)
}

pub fn post_processing(ec: &mut ExpressionChronicle, sym_table: &mut SymTable, context: &Context) {
    unify_equal(ec, sym_table, context);
    rm_useless_var(ec, sym_table, context);
}

pub fn translate_domain_env_to_hierarchy(context: Context) -> Result<(Domain, SymTable), LError> {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let mut symbol_table = SymTable::default();

    let actions: Vec<String> = context.domain.get_actions().keys().cloned().collect();
    let tasks: Vec<String> = context.domain.get_tasks().keys().cloned().collect();
    let state_functions = context
        .domain
        .get_state_functions()
        .keys()
        .cloned()
        .collect();
    let methods = context.domain.get_methods().keys().cloned().collect();

    symbol_table.add_list_of_symbols_of_same_type(actions, &AtomType::Action)?;
    symbol_table.add_list_of_symbols_of_same_type(state_functions, &AtomType::StateFunction)?;
    symbol_table.add_list_of_symbols_of_same_type(methods, &AtomType::Method)?;
    symbol_table.add_list_of_symbols_of_same_type(tasks, &AtomType::Task)?;

    //Add actions, tasks and methods symbols to symbol_table:
    #[allow(unused_mut)]
    let mut methods = vec![];
    #[allow(unused_mut)]
    let mut tasks = vec![];
    #[allow(unused_mut)]
    let mut actions = vec![];

    for (action_label, action) in context.domain.get_actions() {
        //evaluate the lambda sim.
        let mut chronicle =
            translate_lvalue_to_chronicle(action.get_sim(), &context, &mut symbol_table)?;
        let symbol_id = *symbol_table
            .id(action_label)
            .unwrap_or_else(|| panic!("{} was not well defined", action_label));
        let mut name = vec![symbol_id];

        for e in action.get_parameters().get_params() {
            let symbol_id = *symbol_table
                .id(&e)
                .expect("parameters were not defined in the chronicle");
            name.push(symbol_id);
            chronicle.add_var(&symbol_id);
        }

        chronicle.set_name(name);

        actions.push(chronicle)
    }

    /*//Add all methods to the domain
    for (method_label, method) in context.domain.get_methods() {
        let mut chronicle =
            translate_lvalue_to_chronicle(method.get_body(), &context, &mut symbol_table)?;

        //Declaring the method
        {
            let pre_conditions = translate_lvalue_to_expression_chronicle(
                method.get_pre_conditions(),
                &context,
                &mut symbol_table,
            )?;

            chronicle.absorb_expression_chronicle(pre_conditions);

            let symbol_id = *symbol_table
                .id(method_label)
                .unwrap_or_else(|| panic!("{} was not well defined", method_label));
            let mut name = vec![symbol_id];

            for e in method.get_parameters().get_params() {
                let symbol_id = *symbol_table
                    .id(&e)
                    .expect("parameters were not defined in the chronicle");
                name.push(symbol_id);
                chronicle.add_var(&symbol_id);
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
    }*/

    Ok((Domain::new(actions, tasks, methods), symbol_table))
}

pub fn translate_lvalue_to_chronicle(
    exp: &LValue,
    context: &Context,
    symbol_table: &mut SymTable,
) -> Result<Chronicle, LError> {
    //Creation and instantiation of the chronicle

    if let LValue::Lambda(lambda) = exp {
        let mut chronicle = Chronicle::default();
        let params = lambda.get_params();
        match params {
            LambdaArgs::Sym(s) => {
                symbol_table.declare_new_symbol(s, true);
            }
            LambdaArgs::List(list) => {
                for param in list {
                    symbol_table.declare_new_symbol(param, true);
                }
            }
            LambdaArgs::Nil => {}
        }
        let body = pre_processing(&lambda.get_body(), context)?;

        chronicle.set_debug(Some(body.clone()));

        let mut ec = translate_lvalue_to_expression_chronicle(&body, context, symbol_table)?;

        post_processing(&mut ec, symbol_table, context);

        chronicle.absorb_expression_chronicle(ec);
        Ok(chronicle)
    } else {
        Err(SpecialError(
            TRANSLATE_LVALUE_TO_CHRONICLE,
            format!(
                "chronicle can only be extracted from a lambda, here we have a {}.",
                NameTypeLValue::from(exp)
            ),
        ))
    }
}

pub fn translate_lvalue_to_expression_chronicle(
    exp: &LValue,
    context: &Context,
    symbol_table: &mut SymTable,
) -> Result<ExpressionChronicle, LError> {
    let mut ec = ExpressionChronicle::new(exp.clone(), symbol_table);

    match exp {
        LValue::Symbol(s) => {
            //Generale case
            ec.set_lit(symbol_table.declare_new_symbol(s.into(), false).into());
            ec.add_constraint(Constraint::Eq(ec.get_result().into(), ec.get_lit()));
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    let var = symbol_table.declare_new_symbol(l[1].to_string(), true);
                    let val =
                        translate_lvalue_to_expression_chronicle(&l[2], context, symbol_table)?;
                    ec.add_constraint(Constraint::Eq(val.get_result().into(), var.into()));
                    ec.add_constraint(Constraint::Eq(
                        ec.get_result().into(),
                        symbol_table.new_bool(false).into(),
                    ));
                    ec.absorb(val);
                }
                LCoreOperator::Begin => {
                    symbol_table.new_scope();
                    let mut literal: Vec<Lit> = vec![symbol_table
                        .id(BEGIN)
                        .expect("begin is not defined in the symbol table")
                        .into()];
                    let mut previous_interval = *ec.get_interval();
                    for (_i, e) in l[1..].iter().enumerate() {
                        let mut ec_i =
                            translate_lvalue_to_expression_chronicle(e, context, symbol_table)?;

                        literal.push(ec_i.get_result().into());

                        ec_i.add_constraint(Constraint::LT(
                            previous_interval.end().into(),
                            ec_i.get_interval().start().into(),
                        ));

                        previous_interval = *ec_i.get_interval();

                        /*if i == l.len() - 2 {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_i.get_result().into(),
                            ))
                        }*/
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    let literal: Lit = literal.into();

                    let literal: Lit = vec![
                        symbol_table
                            .id(EVAL)
                            .expect("eval not defined in symbol table.")
                            .into(),
                        literal,
                    ]
                    .into();

                    ec.set_lit(literal);

                    ec.add_effect(Effect {
                        interval: *ec.get_interval(),
                        transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                    });
                    //ec.add_subtask(expression_chronicle);
                    symbol_table.revert_scope();
                }
                LCoreOperator::If => {
                    //We suppose here that the condition is a pure literal condition
                    let cond = &l[1];
                    let a = &l[2];
                    let b = &l[3];

                    let cond = translate_cond_if(cond, context, symbol_table)?;

                    //Taking in account 1 branch if
                    if b == &LValue::Nil {
                        ec.add_condition(Condition {
                            interval: Interval::new(
                                &ec.get_interval().start(),
                                &ec.get_interval().start(),
                            ),
                            constraint: Constraint::Eq(cond, symbol_table.new_bool(true).into()),
                        });

                        let ec_a =
                            translate_lvalue_to_expression_chronicle(a, context, symbol_table)?;

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
                            constraint: Constraint::Neg(cond),
                        });

                        let ec_b =
                            translate_lvalue_to_expression_chronicle(b, context, symbol_table)?;

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
                            TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                            "Two branches If block is not supported yet...".to_string(),
                        ));
                    }
                }
                LCoreOperator::Quote => {
                    ec = translate_lvalue_to_expression_chronicle(&l[1], context, symbol_table)?;
                }
                co => {
                    return Err(SpecialError(
                        TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                        format!("{} not supported yet", co),
                    ))
                }
            },
            _ => {
                let mut expression_type = ExpressionType::Lisp;

                symbol_table.new_scope();
                let mut literal: Vec<Lit> = vec![];

                if let LValue::Symbol(s) = &l[0] {
                    match s.as_str() {
                        RAE_ASSERT => {
                            if l.len() != 3 {
                                return Err(WrongNumberOfArgument(
                                    TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                                    exp.clone(),
                                    l.len(),
                                    3..3,
                                ));
                            }

                            let state_variable = translate_lvalue_to_expression_chronicle(
                                &l[1],
                                context,
                                symbol_table,
                            )?;
                            let value = translate_lvalue_to_expression_chronicle(
                                &l[2],
                                context,
                                symbol_table,
                            )?;

                            ec.add_effect(Effect {
                                interval: *ec.get_interval(),
                                transition: Transition::new(
                                    state_variable.get_lit(),
                                    value.get_lit(),
                                ),
                            });

                            ec.absorb(state_variable);
                            ec.absorb(value);

                            return Ok(ec);
                        }
                        //RAE_INSTANCE => {}
                        RAE_RETRACT => {
                            return Err(SpecialError(
                                TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                                "not yet supported".to_string(),
                            ))
                        }
                        _ => {
                            if let Some(id) = symbol_table.id(s) {
                                match symbol_table
                                        .get_type(id)
                                        .expect("a defined symbol should have a type")
                                    {
                                        AtomType::Action => {
                                            expression_type = ExpressionType::Action;
                                        }
                                        AtomType::Function => {
                                        }
                                        AtomType::Method => return Err(SpecialError(TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{} is method and can not be directly called into the body of a method.\
                                \nPlease call the task that use the method instead", s))),
                                        AtomType::StateFunction => {
                                            expression_type = ExpressionType::StateFunction
                                        }
                                        AtomType::Task => {
                                            expression_type = ExpressionType::Task
                                        }
                                        _ => return Err(SpecialError(TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE, "{} first symbol should be a function, task, action or state function".to_string())),
                                    }
                                literal.push(id.into())
                            }
                        }
                    }
                }

                let mut previous_interval = *ec.get_interval();
                for e in &l[1..] {
                    let mut ec_i =
                        translate_lvalue_to_expression_chronicle(e, context, symbol_table)?;

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

                match expression_type {
                    ExpressionType::Pure | ExpressionType::Lisp => {
                        let literal: Lit = vec![
                            symbol_table
                                .id(EVAL)
                                .expect("Eval not defined in symbol table")
                                .into(),
                            Lit::from(literal),
                        ]
                        .into();

                        ec.set_lit(literal);

                        ec.add_effect(Effect {
                            interval: *ec.get_interval(),
                            transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                        });
                        //ec.add_subtask(expression_chronicle);
                    }
                    ExpressionType::Action | ExpressionType::Task => ec.add_subtask(Expression {
                        interval: *ec.get_interval(),
                        lit: literal.into(),
                    }),
                    ExpressionType::StateFunction => {
                        ec.set_lit(literal.into());

                        ec.add_effect(Effect {
                            interval: *ec.get_interval(),
                            transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                        });
                    }
                }
                symbol_table.revert_scope();
            }
        },
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_lit(lvalue_to_lit(exp, symbol_table)?);
            ec.add_constraint(Constraint::Eq(ec.get_result().into(), ec.get_lit()))
        }
        lv => {
            return Err(SpecialError(
                TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                format!("{} not supported yet", NameTypeLValue::from(lv)),
            ))
        }
    }

    Ok(ec)
}

pub const TRANSLATE_COND_IF: &str = "translate_cond_if";

pub fn translate_cond_if(
    exp: &LValue,
    context: &Context,
    symbol_table: &mut SymTable,
) -> Result<Lit, LError> {
    match exp {
        LValue::List(list) => {
            if list.len() == 4 {
                if let LValue::CoreOperator(LCoreOperator::If) = &list[0] {
                    let cond = translate_cond_if(&list[1], context, symbol_table)?;

                    //And case:
                    if list[3] == LValue::Nil {
                        Ok(Constraint::And(
                            cond,
                            translate_cond_if(&list[2], context, symbol_table)?,
                        )
                        .into())
                    }
                    //Or case
                    else if list[2] == LValue::True {
                        Ok(Constraint::Or(
                            cond,
                            translate_cond_if(&list[3], context, symbol_table)?,
                        )
                        .into())
                    } else {
                        Err(SpecialError(
                            TRANSLATE_COND_IF,
                            "Expected an \"or\" or \"and\" expression.".to_string(),
                        ))
                    }
                } else {
                    Err(WrongType(
                        TRANSLATE_COND_IF,
                        list[0].clone(),
                        (&list[0]).into(),
                        NameTypeLValue::CoreOperator,
                    ))
                }
            } else {
                Ok(lvalue_to_lit(exp, symbol_table)?)
            }
        }
        LValue::Symbol(s) => Ok(symbol_table.declare_new_symbol(s.clone(), false).into()),
        LValue::Character(c) => Ok(symbol_table.declare_new_symbol(c.to_string(), false).into()),
        LValue::Nil | LValue::True | LValue::Number(_) => Ok(lvalue_to_lit(exp, symbol_table)?),
        _ => Err(NotInListOfExpectedTypes(
            TRANSLATE_COND_IF,
            exp.clone(),
            exp.into(),
            vec![NameTypeLValue::List, NameTypeLValue::Atom],
        )),
    }
}

pub const TRANSFORM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub fn transform_lambda_expression(lv: &LValue, env: LEnv, ctxs: ContextCollection) -> LResult {
    //println!("in transform lambda");

    if let LValue::List(list) = lv {
        if list.is_empty() {
            return Err(WrongNumberOfArgument(
                TRANSFORM_LAMBDA_EXPRESSION,
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
                            TRANSFORM_LAMBDA_EXPRESSION,
                            format!(
                                "in lambda {}: ",
                                WrongNumberOfArgument(
                                    TRANSFORM_LAMBDA_EXPRESSION,
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
                            TRANSFORM_LAMBDA_EXPRESSION,
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
                TRANSFORM_LAMBDA_EXPRESSION,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Lambda,
            ))
        }
    } else {
        Err(WrongType(
            TRANSFORM_LAMBDA_EXPRESSION,
            lv.clone(),
            lv.into(),
            NameTypeLValue::List,
        ))
    }
}
