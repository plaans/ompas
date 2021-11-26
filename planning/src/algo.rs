use crate::structs::Lit;
use crate::structs::*;
use ompas_lisp::functions::last;
use ompas_lisp::language::scheme_primitives::EVAL;
use ompas_lisp::structs::{LCoreOperator, LValue};

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
                    ec.get_interval().clone(),
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
                    ec = ec.absorb(val);
                }
                LCoreOperator::Begin => {
                    sym_table.new_context();
                    let mut literal: Vec<Lit> = vec!["begin".into()];
                    let mut previous_interval = ec.get_interval().clone();
                    for (i, e) in l[1..].iter().enumerate() {
                        let mut ec_i = translate_lvalue_to_expression_chronicle_r(e, sym_table);

                        literal.push(ec_i.get_result().into());

                        ec_i.add_constraint(Constraint::LT(
                            previous_interval.end().into(),
                            ec_i.get_interval().start().into(),
                        ));

                        previous_interval = ec_i.get_interval().clone();

                        if i == l.len() - 2 {
                            ec.add_constraint(Constraint::Eq(
                                ec.get_result().into(),
                                ec_i.get_result().into(),
                            ))
                        }
                        ec = ec.absorb(ec_i);
                    }

                    ec.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec.get_interval().end().into(),
                    ));

                    let literal: Lit = vec![EVAL.into(), literal.into()].into();

                    let expression_chronicle = Expression {
                        interval: ec.get_interval().clone(),
                        lit: literal.clone(),
                    };

                    ec.set_lit(literal);
                    let persistence = sym_table.declare_new_timepoint();

                    ec.add_var(&persistence);
                    ec.add_effect(Effect {
                        interval: TransitionInterval::new(ec.get_interval().clone(), persistence),
                        transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                    });
                    //ec.add_subtask(expression_chronicle);
                    sym_table.revert_context();
                }
                _ => panic!("not supported yet"),
            },
            _ => {
                sym_table.new_context();
                let mut literal: Vec<Lit> = vec![];
                let mut previous_interval = ec.get_interval().clone();
                for e in l {
                    let mut ec_i = translate_lvalue_to_expression_chronicle_r(e, sym_table);

                    literal.push(ec_i.get_result().into());

                    ec_i.add_constraint(Constraint::LT(
                        previous_interval.end().into(),
                        ec_i.get_interval().start().into(),
                    ));

                    previous_interval = ec_i.get_interval().clone();
                    ec = ec.absorb(ec_i);
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
                    interval: TransitionInterval::new(ec.get_interval().clone(), persistence),
                    transition: Transition::new(ec.get_result().into(), ec.get_lit()),
                });
                //ec.add_subtask(expression_chronicle);
                sym_table.revert_context();
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

    let mut previous_interval = interval.clone();
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

        previous_interval = expression.interval.clone();

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
                    .format_with_sym_table(&sym_table)
                    .to_string()
                    .into(),
            ])
            .into(),
        ),
    });
    chronicle.add_subtask(expression_chronicle);
    chronicle
}
