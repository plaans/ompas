use crate::conversion::post_processing::post_processing;
use crate::structs::atom::Sym;
use crate::structs::chronicle::ChronicleTemplate;
use crate::structs::condition::Condition;
use crate::structs::constraint::*;
use crate::structs::effect::Effect;
use crate::structs::expression::Expression;
use crate::structs::expression_chronicle::ExpressionChronicle;
use crate::structs::interval::Interval;
use crate::structs::lit::{lvalue_to_lit, Lit};
use crate::structs::symbol_table::{AtomId, ExpressionType};
use crate::structs::traits::{Absorb, FormatWithSymTable, GetVariables};
use crate::structs::type_table::{AtomKind, PlanningAtomType, VariableKind};
use crate::structs::{ConversionCollection, ConversionContext, TaskType};
use aries_planning::chronicles::ChronicleKind;
use ompas_rae_language::*;
use ompas_rae_structs::context::*;
use sompas_core::static_eval::{eval_static, parse_static};
use sompas_language::*;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;
use sompas_structs::{lerror, wrong_n_args};
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;

//Names of the functions

//const PRE_PROCESSING: &str = "pre_processing";
const CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE: &str = "convert_lvalue_to_expression_chronicle";

#[derive(Default, Clone, Copy)]
pub struct MetaData {
    top_level: bool,
    check_into_condition: bool,
}

impl MetaData {
    pub fn new(top_level: bool, check_into_condition: bool) -> Self {
        Self {
            top_level,
            check_into_condition,
        }
    }

    pub fn is_inside_do(&mut self) {
        if self.top_level {
            self.check_into_condition = true;
            self.top_level = false;
        } else {
            self.check_into_condition &= true;
        }
    }

    pub fn is_last_of_begin(&mut self) {
        if self.top_level {
            self.check_into_condition = true;
            self.top_level = false;
        } else {
            self.check_into_condition &= true;
        }
    }
}

pub fn convert_lvalue_to_expression_chronicle(
    exp: &LValue,
    context: &ConversionContext,
    ch: &mut ConversionCollection,
    meta_data: MetaData,
) -> lerror::Result<ExpressionChronicle> {
    let mut ec = ExpressionChronicle::new(exp.clone(), &mut ch.sym_table);

    match exp {
        LValue::Symbol(s) => {
            //General case
            let symbol = ch.sym_table.declare_symbol(s, None);
            if ch.sym_table.get_type_of(&symbol).unwrap().kind
                == AtomKind::Variable(VariableKind::Parameter)
            {
                ec.add_var(&symbol);
            }
            ec.set_pure_result(symbol.into());

            ec.make_instantaneous();
        }
        LValue::Nil | LValue::True | LValue::Number(_) | LValue::String(_) => {
            ec.set_pure_result(lvalue_to_lit(exp, &mut ch.sym_table)?);
            //As the result is pure, the expression is considering as having a null time of execution.
            ec.make_instantaneous();
        }
        LValue::List(l) => match &l[0] {
            LValue::CoreOperator(co) => match co {
                LCoreOperator::Define => {
                    //Todo : handle the case when the first expression is not a symbol, but an expression that must be evaluated
                    if let LValue::Symbol(s) = &l[1] {
                        let var = ch
                            .sym_table
                            .declare_new_variable(s, None, VariableKind::Local);
                        let val = convert_lvalue_to_expression_chronicle(
                            &l[2],
                            context,
                            ch,
                            Default::default(),
                        )?;
                        if val.is_result_pure() {
                            ec.make_instantaneous()
                        }

                        ec.add_constraint(finish(val.get_interval(), ec.get_interval()));
                        ec.add_constraint(Constraint::Eq(var.into(), val.get_result_as_lit()));
                        ec.set_pure_result(ch.sym_table.new_bool(false).into());
                        ec.absorb(val);
                    } else {
                        return Err(lerror!(
                            CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!(
                                "Define should take as first argument a symbol and {} is not.",
                                l[1]
                            )
                        ));
                    }
                }
                LCoreOperator::Do => {
                    ch.sym_table.new_scope();
                    let mut literal: Vec<Lit> = vec![ch
                        .sym_table
                        .id(co.to_string().as_str())
                        .unwrap_or_else(|| panic!("{} is not defined in the symbol table", co))
                        .into()];
                    let mut previous_interval: Interval = *ec.get_interval();

                    /*
                    Harcoded solution that needs to be improved in further iteration of the analysis
                     */
                    let mut meta_data = meta_data;
                    meta_data.is_inside_do();
                    for (i, e) in l[1..].iter().enumerate() {
                        let ec_i =
                            convert_lvalue_to_expression_chronicle(e, context, ch, meta_data)?;

                        literal.push(ec_i.get_result_as_lit());

                        if i == l.len() - 2 {
                            if ec_i.is_result_pure() {
                                ec.set_pure_result(ec_i.get_result_as_lit())
                            } else {
                                ec.add_constraint(bind_result(&ec, &ec_i));
                            }
                        }

                        //Temporal constraints
                        if i == 0 {
                            ec.add_constraint(start(&previous_interval, ec_i.get_interval()));
                        } else {
                            ec.add_constraint(meet(&previous_interval, ec_i.get_interval()));
                        }
                        previous_interval = *ec_i.get_interval();

                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(finish(&previous_interval, ec.get_interval()));

                    ch.sym_table.revert_scope();
                }
                LCoreOperator::Begin => {
                    ch.sym_table.new_scope();
                    let mut literal: Vec<Lit> = vec![ch
                        .sym_table
                        .id(co.to_string().as_str())
                        .unwrap_or_else(|| panic!("{} is not defined in the symbol table", co))
                        .into()];
                    let mut previous_interval: Interval = *ec.get_interval();

                    for (i, e) in l[1..].iter().enumerate() {
                        let mut meta_data = meta_data;

                        if i == l.len() - 2 {
                            meta_data.is_last_of_begin();
                        }

                        let ec_i =
                            convert_lvalue_to_expression_chronicle(e, context, ch, meta_data)?;

                        literal.push(ec_i.get_result_as_lit());
                        if i == 0 {
                            ec.add_constraint(start(&previous_interval, ec_i.get_interval()));
                        } else {
                            ec.add_constraint(meet(&previous_interval, ec_i.get_interval()));
                        }

                        if i == l.len() - 2 {
                            ec.set_pure_result(ec_i.get_result_as_lit())
                        } else {
                            ec.add_constraint(bind_result(&ec, &ec_i));
                        }

                        previous_interval = *ec_i.get_interval();

                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(finish(&previous_interval, ec.get_interval()));

                    ch.sym_table.revert_scope();
                }
                LCoreOperator::If => {
                    return convert_if(exp, context, ch);
                }
                LCoreOperator::Quote => {
                    ec.set_pure_result(lvalue_to_lit(&l[1], &mut ch.sym_table)?);
                    ec.make_instantaneous();
                }
                co => {
                    return Err(lerror!(
                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                        format!("{} not supported yet\nexp : {}", co, exp)
                    ))
                }
            },
            _ => {
                let mut expression_type = ExpressionType::Lisp;
                let mut is_special_expression = false;
                ch.sym_table.new_scope();
                let mut literal: Vec<Lit> = vec![];

                match &l[0] {
                    LValue::Symbol(_) | LValue::Fn(_) => {
                        let s = l[0].to_string();
                        let str = s.as_str();
                        match str {
                            RAE_MONITOR => {
                                //A monitor is a condition
                                let fluent = convert_lvalue_to_expression_chronicle(
                                    &l[1],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;
                                ec.add_constraint(equal(ec.get_interval(), fluent.get_interval()));

                                //WARNING: revoir la définition de la conversion de monitor
                                /*ec.add_condition(Condition {
                                    interval: Interval::new_instantaneous(&ec.get_interval().end()),
                                    constraint: bind_result(&ec, &fluent),
                                });*/

                                let r: Vec<AtomId> = fluent.get_result_as_lit().try_into()?;

                                ec.add_condition(Condition {
                                    interval: Interval::new_instantaneous(ec.get_end()),
                                    sv: r,
                                    value: ch.sym_table.new_bool(true),
                                });

                                ec.set_pure_result(ch.sym_table.new_bool(true).into());

                                ec.absorb(fluent);
                                is_special_expression = true;
                            }
                            RAE_ASSERT | RAE_ASSERT_SHORT => {
                                if l.len() != 3 {
                                    return Err(wrong_n_args!(
                                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                        l.as_slice(),
                                        3
                                    ));
                                }

                                let state_variable = convert_lvalue_to_expression_chronicle(
                                    &l[1],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;
                                let value = convert_lvalue_to_expression_chronicle(
                                    &l[2],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;

                                //Temporal constraints
                                ec.add_constraint(start(
                                    ec.get_interval(),
                                    state_variable.get_interval(),
                                ));
                                ec.add_constraint(meet(
                                    state_variable.get_interval(),
                                    value.get_interval(),
                                ));

                                //instantaneous effect;
                                /*ec.add_constraint(finish(value.get_interval(), ec.get_interval()));
                                ec.add_effect(Effect {
                                    interval: Interval::new_instantaneous(ec.get_end()),
                                    sv: state_variable.get_result_as_lit().try_into()?,
                                    value: value.get_result_as_lit().try_into()?,
                                });*/

                                //Not instantaneous effect.
                                ec.add_constraint(Constraint::lt(value.get_end(), ec.get_end()));
                                ec.add_effect(Effect {
                                    interval: Interval::new_instantaneous(ec.get_end()),
                                    sv: state_variable.get_result_as_lit().try_into()?,
                                    value: value.get_result_as_lit().try_into()?,
                                });

                                ec.absorb(state_variable);
                                ec.absorb(value);

                                ec.set_pure_result(ch.sym_table.new_bool(true).into());
                                is_special_expression = true;
                            }
                            RAE_RETRACT | RAE_RETRACT_SHORT => {
                                return Err(lerror!(
                                    CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                    "not yet supported"
                                ))
                            }
                            EQ | NEQ | GT | GEQ | LT | LEQ => {
                                let left = convert_lvalue_to_expression_chronicle(
                                    &l[1],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;
                                let right = convert_lvalue_to_expression_chronicle(
                                    &l[2],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;

                                ec.add_constraint(start(ec.get_interval(), left.get_interval()));
                                ec.add_constraint(meet(left.get_interval(), right.get_interval()));
                                ec.add_constraint(finish(right.get_interval(), ec.get_interval()));

                                let a = left.get_result_as_lit();
                                let b = right.get_result_as_lit();

                                let constraint = match str {
                                    EQ => Constraint::Eq(a, b),
                                    LT => Constraint::Lt(a, b),
                                    GT => Constraint::Lt(b, a),
                                    LEQ => Constraint::Leq(a, b),
                                    GEQ => Constraint::Leq(b, a),
                                    NEQ => Constraint::Neq(a, b),
                                    _ => unreachable!(),
                                };

                                if !meta_data.top_level
                                    && left.is_result_pure()
                                    && right.is_result_pure()
                                {
                                    ec.set_pure_result(constraint.into());
                                } else {
                                    /*ec.add_condition(Condition {
                                        interval: *ec.get_interval(),
                                        constraint: Constraint::Eq(
                                            ec.get_result_as_lit(),
                                            constraint.into(),
                                        ),
                                    });*/
                                    ec.add_constraint(Constraint::Eq(
                                        ec.get_result_as_lit(),
                                        constraint.into(),
                                    ));
                                    ch.sym_table.set_type_of(
                                        ec.get_result_id(),
                                        &Some(PlanningAtomType::Bool),
                                    );
                                }
                                ec.absorb(left);
                                ec.absorb(right);
                                is_special_expression = true;
                            }
                            ARBITRARY => {
                                let val = convert_lvalue_to_expression_chronicle(
                                    &l[1],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;

                                ec.add_constraint(equal(ec.get_interval(), val.get_interval()));
                                let constraint = Constraint::Arbitrary(
                                    ec.get_result_id().into(),
                                    val.get_result_as_lit(),
                                );

                                ec.add_constraint(constraint);

                                /*if !meta_data.top_level && val.is_result_pure() {
                                    ec.add_constraint(constraint);
                                } else {
                                    ec.add_constraint(constraint);
                                    /*ec.add_condition(Condition {
                                        interval: Interval::new_instantaneous(ec.get_end()),
                                        constraint: constraint,
                                    });*/
                                }*/

                                ec.absorb(val);
                                is_special_expression = true;
                            }
                            NOT | NOT_SHORT | IS_BOOL | IS_FLOAT | IS_INT | IS_LIST | IS_NUMBER
                            | IS_NIL => {
                                let val = convert_lvalue_to_expression_chronicle(
                                    &l[1],
                                    context,
                                    ch,
                                    Default::default(),
                                )?;

                                ec.add_constraint(equal(ec.get_interval(), val.get_interval()));

                                let r = val.get_result_as_lit();

                                let constraint = match str {
                                    NOT | NOT_SHORT => Constraint::Not(r),
                                    IS_BOOL => {
                                        Constraint::Type(r, ch.sym_table.id(BOOL).unwrap().into())
                                    }
                                    IS_FLOAT => {
                                        Constraint::Type(r, ch.sym_table.id(FLOAT).unwrap().into())
                                    }
                                    IS_INT => {
                                        Constraint::Type(r, ch.sym_table.id(INT).unwrap().into())
                                    }
                                    IS_LIST => Constraint::Type(
                                        r,
                                        ch.sym_table.id(TYPE_LIST).unwrap().into(),
                                    ),
                                    IS_NUMBER => {
                                        Constraint::Type(r, ch.sym_table.id(NUMBER).unwrap().into())
                                    }
                                    IS_NIL => {
                                        Constraint::Eq(r, ch.sym_table.new_bool(false).into())
                                    }
                                    _ => unreachable!(),
                                };
                                if !meta_data.top_level && val.is_result_pure() {
                                    if meta_data.top_level {
                                        ec.add_constraint(Constraint::Eq(
                                            ec.get_result_as_lit(),
                                            constraint.into(),
                                        ));
                                        ch.sym_table.set_type_of(
                                            ec.get_result_id(),
                                            &Some(PlanningAtomType::Bool),
                                        );
                                    } else {
                                        ec.set_pure_result(constraint.into());
                                    }
                                } else {
                                    /*ec.add_condition(Condition {
                                        interval: *ec.get_interval(),
                                        constraint: Constraint::Eq(
                                            ec.get_result_as_lit(),
                                            constraint.into(),
                                        ),
                                    });*/
                                    ec.add_constraint(Constraint::Eq(
                                        ec.get_result_as_lit(),
                                        constraint.into(),
                                    ));
                                    ch.sym_table.set_type_of(
                                        ec.get_result_id(),
                                        &Some(PlanningAtomType::Bool),
                                    );
                                }
                                ec.absorb(val);
                                is_special_expression = true;
                            }
                            RAE_INSTANCE => {
                                match l.len() {
                                    2 => {
                                        //Case to return the list of all instances of a type
                                        let symbol_type = convert_lvalue_to_expression_chronicle(
                                            &l[1],
                                            context,
                                            ch,
                                            Default::default(),
                                        )?;

                                        ec.add_constraint(equal(
                                            symbol_type.get_interval(),
                                            ec.get_interval(),
                                        ));

                                        if symbol_type.is_result_pure() {
                                            let key = format!(
                                                "(instance {})",
                                                symbol_type.format(&ch.sym_table, false)
                                            );
                                            let lvalue =
                                                parse_static(&key, &mut context.env.clone())?;
                                            let result = match context
                                                .state
                                                ._static
                                                .get(&lvalue.get_lvalue().into())
                                            {
                                                Some(r) => r.into(),
                                                None => LValue::Nil,
                                            };
                                            ec.add_constraint(Constraint::Eq(
                                                ec.get_result_as_lit(),
                                                lvalue_to_lit(&result, &mut ch.sym_table)?,
                                            ));
                                        } else {
                                            return Err(lerror!(CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE, "cannot handle (instance <type>) with <type> undecided"));
                                        }
                                    }
                                    3 => {
                                        let symbol = convert_lvalue_to_expression_chronicle(
                                            &l[1],
                                            context,
                                            ch,
                                            Default::default(),
                                        )?;
                                        let symbol_type = convert_lvalue_to_expression_chronicle(
                                            &l[2],
                                            context,
                                            ch,
                                            Default::default(),
                                        )?;

                                        ec.add_constraint(start(
                                            ec.get_interval(),
                                            symbol.get_interval(),
                                        ));
                                        ec.add_constraint(meet(
                                            symbol.get_interval(),
                                            symbol_type.get_interval(),
                                        ));
                                        ec.add_constraint(finish(
                                            symbol_type.get_interval(),
                                            ec.get_interval(),
                                        ));

                                        /*let constraint = Constraint::Type(
                                            symbol.get_result_as_lit(),
                                            symbol_type.get_result_as_lit(),
                                        );*/
                                        let r = ch.sym_table.declare_new_result(Some(
                                            PlanningAtomType::SubType(
                                                *ch.sym_table.get_type_id(OBJECT).unwrap(),
                                            ),
                                        ));
                                        ec.add_var(&r);
                                        ec.add_condition(Condition {
                                            interval: *ec.get_interval(),
                                            sv: vec![
                                                *ch.sym_table.id(RAE_INSTANCE).unwrap(),
                                                symbol.get_result_as_lit().try_into()?,
                                            ],
                                            value: r,
                                        });
                                        ec.add_constraint(Constraint::Eq(
                                            ec.get_result_as_lit(),
                                            Constraint::Eq(
                                                r.into(),
                                                symbol_type.get_result_as_lit(),
                                            )
                                            .into(),
                                        ));

                                        /*ec.add_condition(Condition {
                                            interval: *ec.get_interval(),
                                            constraint: Constraint::Eq(
                                                ec.get_result_as_lit(),
                                                constraint.into(),
                                            ),
                                        });*/
                                        ch.sym_table.set_type_of(
                                            ec.get_result_id(),
                                            &Some(PlanningAtomType::Bool),
                                        );
                                        ec.absorb(symbol);
                                        ec.absorb(symbol_type);
                                        is_special_expression = true;
                                    }
                                    _ => {
                                        return Err(lerror!(
                                            CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                            format!(
                                            "{} has not the right number of args (expecting 1..2)",
                                            exp
                                        )
                                        ))
                                    }
                                }
                            }
                            CHECK => {
                                if meta_data.check_into_condition {
                                    let condition = convert_lvalue_to_expression_chronicle(
                                        &l[1],
                                        context,
                                        ch,
                                        Default::default(),
                                    )?;
                                    match condition.get_result_as_lit() {
                                        Lit::Constraint(c) => ec.add_constraint(c.deref().clone()),
                                        l => ec.add_constraint(Constraint::Eq(
                                            l,
                                            ch.sym_table.new_bool(true).into(),
                                        )),
                                    }

                                    ec.add_constraint(equal(
                                        ec.get_interval(),
                                        condition.get_interval(),
                                    ));
                                    ch.sym_table.set_type_of(
                                        condition.get_result_id(),
                                        &Some(PlanningAtomType::Bool),
                                    );

                                    //WARNING: Not sure of this
                                    ec.absorb(condition);
                                    ec.set_pure_result(ch.sym_table.new_bool(true).into());

                                    is_special_expression = true;
                                }
                            }
                            _ => {
                                if let Some(id) = ch.sym_table.id(&s) {
                                    match ch.sym_table
                                        .get_type_of(id)
                                        .expect("a defined symbol should have a type").a_type.unwrap()
                                    {
                                        PlanningAtomType::Action => {
                                            expression_type = ExpressionType::Action;
                                            //println!("{} is an action", s);
                                        }
                                        PlanningAtomType::Function => {
                                        }
                                        PlanningAtomType::Method => return Err(lerror!(CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{} is method and can not be directly called into the body of a method.\
                                \nPlease call the task that use the method instead", s))),
                                        PlanningAtomType::StateFunction => {
                                            let (_,return_type) = context.domain.get_state_functions().get(&s).unwrap().get_parameters().inner().last().unwrap();
                                            let return_type = return_type.try_as_single().expect("");
                                            expression_type = ExpressionType::StateFunction(ch.sym_table.str_as_planning_atom_type(return_type))
                                        }
                                        PlanningAtomType::Task => {
                                            expression_type = ExpressionType::Task
                                        }
                                        _ => return Err(lerror!(CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE, format!("{}: first symbol should be a function, task, action or state function", s))),
                                    }
                                    literal.push(id.into())
                                } else {
                                    return Err(lerror!(
                                        CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                                        format!("function {} is not defined", s)
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(lerror!(
                            CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                            format!("{} is not yet supported", KindLValue::from(&l[0]))
                        ))
                    }
                }
                if !is_special_expression {
                    let mut sub_expression_pure = true;

                    let mut previous_interval = *ec.get_interval();
                    let f_symbol_end_timepoint = ch.sym_table.declare_new_timepoint();
                    let mut end_last_interval = f_symbol_end_timepoint;
                    if !matches!(
                        expression_type,
                        ExpressionType::Action | ExpressionType::Task
                    ) {
                        ec.add_constraint(Constraint::Eq(
                            ec.get_interval().start().into(),
                            f_symbol_end_timepoint.into(),
                        ));
                    }
                    for (i, e) in l[1..].iter().enumerate() {
                        let ec_i = convert_lvalue_to_expression_chronicle(
                            e,
                            context,
                            ch,
                            Default::default(),
                        )?;

                        literal.push(ec_i.get_result_as_lit());
                        sub_expression_pure &= ec_i.is_result_pure();
                        if i != 0 {
                            ec.add_constraint(meet(&previous_interval, ec_i.get_interval()));
                        } else {
                            ec.add_constraint(start(&previous_interval, ec_i.get_interval()));
                        }

                        previous_interval = *ec_i.get_interval();
                        end_last_interval = *ec_i.get_interval().end();
                        ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::Leq(
                        end_last_interval.into(),
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
                                    string.push_str(element.format(&ch.sym_table, false).as_str());
                                    string.push(' ');
                                } else {
                                    string.push_str(
                                        format!("(quote {})", element.format(&ch.sym_table, false))
                                            .as_str(),
                                    );
                                }
                            }
                            string.push(')');

                            //println!("static eval of pure expression: {}", string);

                            let result = parse_static(string.as_str(), &mut env);
                            if let Ok(result) = result {
                                if result.is_pure() {
                                    let result = eval_static(result.get_lvalue(), &mut env);

                                    if let Ok(result) = result {
                                        if result.is_pure() {
                                            ec.set_pure_result(lvalue_to_lit(
                                                result.get_lvalue(),
                                                &mut ch.sym_table,
                                            )?);
                                            is_pure = true;
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

                                ec.add_constraint(Constraint::Eq(ec.get_result_as_lit(), literal));
                                /*ec.add_condition(Condition {
                                    interval: *ec.get_interval(),
                                    constraint: Constraint::Eq(ec.get_result_as_lit(), literal),
                                });*/
                                /*ec.add_effect(Effect {
                                    interval: *ec.get_interval(),
                                    transition: Transition::new(ec.get_result_as_lit(), literal),
                                });*/
                            }
                            ec.add_constraint(Constraint::Eq(
                                end_last_interval.into(),
                                ec.get_interval().end().into(),
                            ));
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
                            /*ec.add_condition(Condition {
                                interval: *ec.get_interval(),
                                constraint: Constraint::Eq(ec.get_result_as_lit(), literal),
                            });*/
                            ec.add_constraint(Constraint::Eq(ec.get_result_as_lit(), literal));
                            /*ec.add_effect(Effect {
                                interval: *ec.get_interval(),
                                transition: Transition::new(ec.get_result_as_lit(), literal),
                            });*/
                            ec.add_constraint(Constraint::Eq(
                                end_last_interval.into(),
                                ec.get_interval().end().into(),
                            ));
                        }
                        ExpressionType::Action | ExpressionType::Task => {
                            /*let mut new_literal = vec![
                                ec.get_presence().into(),
                                ec.get_start().into(),
                                ec.get_end().into(),
                                ec.get_result_as_lit(),
                            ];*/
                            //new_literal.append(&mut literal);
                            ec.add_constraint(Constraint::Eq(
                                ec.get_result_as_lit(),
                                ch.sym_table.new_bool(true).into(),
                            ));
                            let t = ch.sym_table.declare_new_timepoint();
                            ec.add_var(&t);
                            let interval = Interval::new(ec.get_start(), &t);
                            ec.add_constraint(Constraint::leq(ec.get_start(), t));
                            ec.add_constraint(Constraint::lt(t, ec.get_end()));
                            ec.add_subtask(Expression {
                                interval,
                                lit: literal.into(),
                            });
                            ch.sym_table
                                .set_type_of(ec.get_result_id(), &Some(PlanningAtomType::Bool));
                        }
                        ExpressionType::StateFunction(return_type) => {
                            ec.add_condition(Condition {
                                interval: *ec.get_interval(),
                                sv: Lit::from(literal).try_into()?,
                                value: ec.get_result_as_lit().try_into()?,
                            });
                            //Return type of the state_function
                            ch.sym_table.set_type_of(ec.get_result_id(), &return_type);
                            ec.add_constraint(Constraint::Eq(
                                end_last_interval.into(),
                                ec.get_interval().end().into(),
                            ));
                        }
                    };
                }
                ch.sym_table.revert_scope();
            }
        },
        lv => {
            return Err(lerror!(
                CONVERT_LVALUE_TO_EXPRESSION_CHRONICLE,
                format!(
                    "{} not supported yet\n exp: {}",
                    KindLValue::from(lv),
                    exp.format(" exp: ".len())
                )
            ))
        }
    }

    Ok(ec)
}

pub fn convert_if(
    exp: &LValue,
    context: &ConversionContext,
    ch: &mut ConversionCollection,
) -> lerror::Result<ExpressionChronicle> {
    ch.sym_table.new_scope();

    let task_label = ch.local_tasks.new_label(TaskType::IfTask);
    ch.sym_table
        .declare_symbol(&task_label, Some(PlanningAtomType::Task));
    //println!("task_label: {}", task_label);
    //println!("({}) expression: \n{}", task_label, exp.format(0));

    let mut ec: ExpressionChronicle = ExpressionChronicle::new(exp.clone(), &mut ch.sym_table);

    let exp: Vec<LValue> = exp
        .try_into()
        .expect("could not transform expression into list");
    let cond = &exp[1];
    let b_true = &exp[2];
    let b_false = &exp[3];

    let b_true_lit = lvalue_to_lit(b_true, &mut ch.sym_table)?;
    let b_false_lit = lvalue_to_lit(b_false, &mut ch.sym_table)?;
    let b_true_variables: im::HashSet<AtomId> = b_true_lit
        .get_variables()
        .iter()
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().kind == AtomKind::Variable(VariableKind::Parameter)
        })
        .cloned()
        .collect();

    let b_false_variables: im::HashSet<AtomId> = b_false_lit
        .get_variables()
        .iter()
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().kind == AtomKind::Variable(VariableKind::Parameter)
        })
        .cloned()
        .collect();

    let union: im::HashSet<AtomId> = b_true_variables.union(b_false_variables);

    //CREATION OF THE TASK
    let mut types = vec![
        /*Some(PlanningAtomType::Bool),
        Some(PlanningAtomType::Timepoint),
        Some(PlanningAtomType::Timepoint),
        None,*/
        Some(PlanningAtomType::Task),
        Some(PlanningAtomType::Bool),
    ];

    let mut task_string = vec![
        /*PREZ.to_string(),
        START.to_string(),
        END.to_string(),
        RESULT.to_string(),*/
        task_label.clone(),
        COND.to_string(),
    ];
    let mut union_string: Vec<String> = vec![];
    for v in &union {
        union_string.push(
            Sym::try_from(ch.sym_table.get_atom(v, false).unwrap())?
                .get_sym()
                .clone(),
        );
        types.push(ch.sym_table.get_type_of(v).and_then(|a| a.a_type));
    }
    task_string.append(&mut union_string);

    let ec_cond = convert_lvalue_to_expression_chronicle(cond, context, ch, Default::default())?;

    let mut task_lit: Vec<AtomId> = vec![
        /* *ec.get_presence(),
         *ec.get_start(),
         *ec.get_end(),
         *ec.get_result_id(),*/
        *ch.sym_table.id(&task_label).unwrap(),
        *ec_cond.get_result_id(),
    ];

    for s in &task_string[2..] {
        task_lit.push(*ch.sym_table.id(s).unwrap());
    }

    let sub_task_interval = ch.sym_table.declare_new_interval();

    ec.add_constraint(Constraint::Leq(
        sub_task_interval.start().into(),
        sub_task_interval.end().into(),
    ));

    /* Temporal constraints between expression that computes the condition
    And the task to execute. */
    ec.add_constraint(start(ec.get_interval(), ec_cond.get_interval()));

    ec.add_constraint(meet(ec_cond.get_interval(), &sub_task_interval));

    ec.add_constraint(finish(ec.get_interval(), &sub_task_interval));

    /* Binding between result of condition and parameter of the task*/
    ec.absorb(ec_cond);
    ec.add_subtask(Expression {
        interval: sub_task_interval,
        lit: task_lit.into(),
    });
    ec.add_interval(&sub_task_interval);

    let mut task_lit: Vec<AtomId> = vec![];
    for (i, s) in task_string.iter().enumerate() {
        if i == 0 {
            task_lit.push(*ch.sym_table.id(s).unwrap());
        } else {
            task_lit.push(ch.sym_table.declare_new_parameter(s, true, types[i]));
        }
    }
    //println!("({}) task lit: {:#?}", task_label, task_lit);
    ch.tasks.push(task_lit);

    let create_method = |lvalue: &LValue,
                         ch: &mut ConversionCollection,
                         branch: bool,
                         debug: LValue|
     -> lerror::Result<()> {
        ch.sym_table.new_scope();
        let method_label = format!("m_{}_{}", task_label, branch);
        let method_id = ch
            .sym_table
            .declare_symbol(&method_label, Some(PlanningAtomType::Method));

        let mut method = ChronicleTemplate::new(ch, &method_label, ChronicleKind::Method);
        let method_cond_var =
            ch.sym_table
                .declare_new_parameter(COND, true, Some(PlanningAtomType::Bool));
        let mut name: Vec<AtomId> = vec![
            /* *method.get_presence(),
             *method.get_start(),
             *method.get_end(),
             *method.get_result(),*/
            method_id,
            method_cond_var,
        ];
        method.add_var(&method_cond_var);
        //Bindings of variables of task and method
        for var in &union {
            let sym: Sym = ch.sym_table.get_atom(var, false).unwrap().try_into()?;
            let new_var = ch.sym_table.declare_new_parameter(
                sym.get_sym(),
                true,
                ch.sym_table.get_type_of(var).unwrap().a_type,
            );
            method.add_var(&new_var);
            name.push(new_var);
        }
        let ec_branch = convert_lvalue_to_expression_chronicle(
            lvalue,
            context,
            ch,
            MetaData::new(true, false),
        )?;
        method.absorb_expression_chronicle(ec_branch);
        method.add_constraint(Constraint::Eq(
            method_cond_var.into(),
            ch.sym_table.new_bool(branch).into(),
        ));

        /*method.add_condition(Condition {
            interval: Interval::new(method.get_start(), method.get_start()),
            constraint: Constraint::Eq(
                method_cond_var.into(),
                ch.sym_table.new_bool(branch).into(),
            ),
        });*/

        let mut task: Vec<AtomId> = name[0..task_string.len()].to_vec();
        task[0] = *ch.sym_table.id(&task_label).unwrap();

        method.set_debug(Some(debug));
        method.set_task(task);
        method.set_name(name);

        //method.absorb_expression_chronicle(ec_branch);
        post_processing(&mut method, context, ch)?;
        ch.chronicle_templates.push(method);
        ch.sym_table.revert_scope();
        Ok(())
    };

    create_method(b_true, ch, true, b_true.clone())?;
    create_method(b_false, ch, false, b_false.clone())?;

    ch.sym_table.revert_scope();
    Ok(ec)

    //Construction of the first method;
}
