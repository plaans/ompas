use crate::structs::atom::AtomType;
use crate::structs::chronicle::ExpressionChronicle;
use crate::structs::condition::Condition;
use crate::structs::constraint::Constraint;
use crate::structs::effect::Effect;
use crate::structs::expression::Expression;
use crate::structs::interval::Interval;
use crate::structs::lit::{lvalue_to_lit, Lit};
use crate::structs::symbol_table::{ExpressionType, SymTable};
use crate::structs::traits::{Absorb, FormatWithSymTable};
use crate::structs::transition::Transition;
use crate::structs::ConversionContext;
#[allow(unused_imports)]
use ompas_acting::rae::module::rae_exec::platform::RAE_INSTANCE;
use ompas_acting::rae::module::rae_exec::{RAE_ASSERT, RAE_RETRACT};
use ompas_lisp::core::structs::lcoreoperator::language::{BEGIN, EVAL};
use ompas_lisp::core::structs::lcoreoperator::LCoreOperator;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::{
    NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_lisp::static_eval::{eval_static, parse_static};

//Names of the functions

//const PRE_PROCESSING: &str = "pre_processing";
const TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE: &str = "translate_lvalue_to_expression_chronicle";

pub const TRANSLATE_COND_IF: &str = "translate_cond_if";

pub fn translate_lvalue_to_expression_chronicle(
    exp: &LValue,
    context: &ConversionContext,
    symbol_table: &mut SymTable,
) -> Result<ExpressionChronicle, LError> {
    let mut ec = ExpressionChronicle::new(exp.clone(), symbol_table);

    match exp {
        LValue::Symbol(s) => {
            //Generale case
            ec.set_pure_result(symbol_table.declare_new_symbol(s.into(), false).into());
        }
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_pure_result(lvalue_to_lit(exp, symbol_table)?);
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    //Todo : handle the case when the first expression is not a symbol, but an expression that must be evaluated
                    if let LValue::Symbol(s) = &l[1] {
                        let var = symbol_table.declare_new_symbol(s.clone(), true);
                        let val =
                            translate_lvalue_to_expression_chronicle(&l[2], context, symbol_table)?;
                        if val.is_result_pure() {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_interval().start().into(),
                                ec.get_interval().end().into(),
                            ))
                        }
                        ec.add_constraint(Constraint::Eq(val.get_result(), var.into()));
                        ec.set_pure_result(symbol_table.new_bool(false).into());
                        ec.absorb(val);
                    } else {
                        return Err(SpecialError(
                            TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!(
                                "Define should take as first argument a symbol and {} is not.",
                                l[1]
                            ),
                        ));
                    }
                }
                LCoreOperator::Begin => {
                    symbol_table.new_scope();
                    let mut literal: Vec<Lit> = vec![symbol_table
                        .id(BEGIN)
                        .expect("begin is not defined in the symbol table")
                        .into()];
                    let mut previous_interval: Interval = *ec.get_interval();

                    for (i, e) in l[1..].iter().enumerate() {
                        let ec_i =
                            translate_lvalue_to_expression_chronicle(e, context, symbol_table)?;

                        literal.push(ec_i.get_result());
                        if i == 0 {
                            ec.add_constraint(Constraint::Eq(
                                previous_interval.end().into(),
                                ec_i.get_interval().start().into(),
                            ));
                        } else {
                            ec.add_constraint(Constraint::Eq(
                                previous_interval.start().into(),
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
                                /*ec.add_constraint(Constraint::Eq(
                                    ec.get_result().into(),
                                    ec_i.get_result().into(),
                                ))*/
                            }
                        }
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::Eq(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    /*let literal: Lit = literal.into();

                    let literal: Lit = vec![
                        symbol_table
                            .id(EVAL)
                            .expect("eval not defined in symbol table.")
                            .into(),
                        literal,
                    ]
                    .into();

                    ec.set_lit(literal);*/

                    symbol_table.revert_scope();
                }
                LCoreOperator::If => {
                    //We suppose here that the condition is a pure literal condition
                    //This supposition is no longer taken in account
                    let cond = &l[1];
                    let a = &l[2];
                    let b = &l[3];

                    let cond_simplification = true;

                    if cond_simplification {
                        let cond = translate_cond_if(cond, context, symbol_table)?;
                        //Taking in account 1 branch if
                        let other_ec;

                        if b == &LValue::Nil {
                            //[e_cond,e_cond] r_cond = true
                            ec.add_condition(Condition {
                                interval: Interval::new(
                                    &ec.get_interval().start(),
                                    &ec.get_interval().start(),
                                ),
                                constraint: Constraint::Eq(
                                    cond,
                                    symbol_table.new_bool(true).into(),
                                ),
                            });

                            other_ec =
                                translate_lvalue_to_expression_chronicle(a, context, symbol_table)?;
                        } else if a == &LValue::Nil {
                            //[e_cond, e_cond] r_cond = false
                            ec.add_condition(Condition {
                                interval: Interval::new(
                                    &ec.get_interval().start(),
                                    &ec.get_interval().start(),
                                ),
                                constraint: Constraint::Neg(cond),
                            });

                            other_ec =
                                translate_lvalue_to_expression_chronicle(b, context, symbol_table)?;
                        }
                        //Two blocks where we have to define two methods
                        else {
                            return Err(SpecialError(
                                TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                                format!(
                                    "Two branches If block is not supported yet...\nexp: {}",
                                    exp
                                ),
                            ));
                        }

                        // e_cond = s_a
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().start().into(),
                            other_ec.get_interval().start().into(),
                        ));
                        //e_a = e
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().end().into(),
                            other_ec.get_interval().end().into(),
                        ));
                        /*ec.add_constraint(Constraint::Eq(
                            ec.get_result().into(),
                            ec_a.get_result().into(),
                        ));*/
                        //[e,e] r <- r_a
                        if other_ec.is_result_pure() {
                            ec.set_pure_result(other_ec.get_result())
                        } else {
                            ec.add_effect(Effect {
                                interval: Interval::new(
                                    &ec.get_interval().end(),
                                    &ec.get_interval().end(),
                                ),
                                transition: Transition::new(ec.get_result(), other_ec.get_result()),
                            });
                        }
                        ec.absorb(other_ec);
                    } else {
                        let ec_cond =
                            translate_lvalue_to_expression_chronicle(cond, context, symbol_table)?;

                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().start().into(),
                            ec_cond.get_interval().start().into(),
                        ));
                        //Taking in account 1 branch if
                        if b == &LValue::Nil {
                            //[e_cond,e_cond] r_cond = true
                            ec.add_condition(Condition {
                                interval: Interval::new(
                                    &ec_cond.get_interval().end(),
                                    &ec_cond.get_interval().end(),
                                ),
                                constraint: Constraint::Eq(
                                    ec_cond.get_result(),
                                    symbol_table.new_bool(true).into(),
                                ),
                            });

                            let ec_a =
                                translate_lvalue_to_expression_chronicle(a, context, symbol_table)?;

                            // e_cond < s_a
                            ec.add_constraint(Constraint::Eq(
                                ec_cond.get_interval().end().into(),
                                ec_a.get_interval().start().into(),
                            ));
                            //e_a = e
                            ec.add_constraint(Constraint::Eq(
                                ec.get_interval().end().into(),
                                ec_a.get_interval().end().into(),
                            ));
                            /*ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_a.get_result().into(),
                            ));*/
                            //[e,e] r <- r_a
                            ec.add_effect(Effect {
                                interval: Interval::new(
                                    &ec.get_interval().end(),
                                    &ec.get_interval().end(),
                                ),
                                transition: Transition::new(ec.get_result(), ec_a.get_result()),
                            });
                            ec.absorb(ec_a);
                        } else if a == &LValue::Nil {
                            //[e_cond, e_cond] r_cond = false
                            ec.add_condition(Condition {
                                interval: Interval::new(
                                    &ec.get_interval().start(),
                                    &ec.get_interval().start(),
                                ),
                                constraint: Constraint::Neg(ec_cond.get_result()),
                            });

                            let ec_b =
                                translate_lvalue_to_expression_chronicle(b, context, symbol_table)?;

                            /*ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_b.get_result().into(),
                            ));*/
                            //e_cond = s_b
                            ec.add_constraint(Constraint::Eq(
                                ec_cond.get_interval().end().into(),
                                ec_b.get_interval().start().into(),
                            ));
                            //e_b = e
                            ec.add_constraint(Constraint::Eq(
                                ec.get_interval().end().into(),
                                ec_b.get_interval().end().into(),
                            ));
                            //[e,e] r <- r_b
                            ec.add_effect(Effect {
                                interval: Interval::new(
                                    &ec.get_interval().end(),
                                    &ec.get_interval().end(),
                                ),
                                transition: Transition::new(ec.get_result(), ec_b.get_result()),
                            });
                            ec.absorb(ec_b);
                        }
                        //Two blocks where we have to define two methods
                        else {
                            return Err(SpecialError(
                                TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                                "Two branches If block is not supported yet...".to_string(),
                            ));
                        }
                        ec.absorb(ec_cond);
                    }
                }
                LCoreOperator::Quote => {
                    ec.set_pure_result(lvalue_to_lit(&l[1], symbol_table)?);
                }
                co => {
                    return Err(SpecialError(
                        TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                        format!("{} not supported yet\nexp : {}", co, exp),
                    ))
                }
            },
            _ => {
                /*let plvalue = eval_static(exp, &mut context.env.clone())?;

                if plvalue.is_pure() {
                    ec.add_constraint(Constraint::Eq(
                        ec.get_result().into(),
                        lvalue_to_lit(plvalue.get_lvalue(), symbol_table)?,
                    ));
                    ec.add_constraint(Constraint::Eq(
                        ec.get_interval().start().into(),
                        ec.get_interval().end().into(),
                    ));
                    return Ok(ec);
                }*/
                let mut expression_type = ExpressionType::Lisp;

                symbol_table.new_scope();
                let mut literal: Vec<Lit> = vec![];

                match &l[0] {
                    LValue::Symbol(_) | LValue::Fn(_) => {
                        let s = l[0].to_string();
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
                                        state_variable.get_result(),
                                        value.get_result(),
                                    ),
                                });

                                ec.absorb(state_variable);
                                ec.absorb(value);

                                ec.set_pure_result(symbol_table.new_bool(false).into());

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
                                if let Some(id) = symbol_table.id(&s) {
                                    match symbol_table
                                        .get_type(id)
                                        .expect("a defined symbol should have a type")
                                    {
                                        AtomType::Action => {
                                            expression_type = ExpressionType::Action;
                                            println!("{} is an action", s);
                                        }
                                        AtomType::Function => {}
                                        AtomType::Method => return Err(SpecialError(TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{} is method and can not be directly called into the body of a method.\
                                \nPlease call the task that use the method instead", s))),
                                        AtomType::StateFunction => {
                                            expression_type = ExpressionType::StateFunction
                                        }
                                        AtomType::Task => {
                                            expression_type = ExpressionType::Task
                                        }
                                        _ => return Err(SpecialError(TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{}: first symbol should be a function, task, action or state function", s))),
                                    }
                                    literal.push(id.into())
                                } else {
                                    return Err(SpecialError(
                                        TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                                        format!("function {} is not defined", s),
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(SpecialError(
                            TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!("{} is not yet supported", TypeLValue::from(&l[0])),
                        ))
                    }
                }

                let mut previous_interval = *ec.get_interval();
                for (i, e) in l[1..].iter().enumerate() {
                    let ec_i = translate_lvalue_to_expression_chronicle(e, context, symbol_table)?;

                    literal.push(ec_i.get_result());
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

                match expression_type {
                    ExpressionType::Pure | ExpressionType::Lisp => {
                        let mut env = context.env.clone();
                        let mut is_pure = false;

                        let mut string = "(".to_string();
                        for (i, element) in literal.iter().enumerate() {
                            if i == 0 {
                                string
                                    .push_str(element.format_with_sym_table(symbol_table).as_str());
                                string.push(' ');
                            } else {
                                string.push_str(
                                    format!(
                                        "(quote {})",
                                        element.format_with_sym_table(symbol_table)
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
                                let result = eval_static(result.get_lvalue(), &mut env);
                                if let Ok(result) = result {
                                    if result.is_pure() {
                                        ec.set_pure_result(lvalue_to_lit(
                                            result.get_lvalue(),
                                            symbol_table,
                                        )?);
                                        is_pure = true;
                                        println!(
                                            "eval static is a success! result is: {}",
                                            result.get_lvalue()
                                        );
                                    }
                                }
                            }
                        } else {
                        }
                        if !is_pure {
                            let literal: Lit = vec![
                                symbol_table
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
                    ExpressionType::Action | ExpressionType::Task => ec.add_subtask(Expression {
                        interval: *ec.get_interval(),
                        lit: literal.into(),
                    }),
                    ExpressionType::StateFunction => {
                        ec.set_lit(literal.into());

                        ec.add_effect(Effect {
                            interval: *ec.get_interval(),
                            transition: Transition::new(ec.get_result(), ec.get_result()),
                        });
                    }
                };
                symbol_table.revert_scope();
            }
        },
        lv => {
            return Err(SpecialError(
                TRANSLATE_LVALUE_TO_EXPRESSION_CHRONICLE,
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

pub fn translate_cond_if(
    exp: &LValue,
    context: &ConversionContext,
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
                        TypeLValue::CoreOperator,
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
            vec![TypeLValue::List, TypeLValue::Atom],
        )),
    }
}
