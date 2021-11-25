use crate::structs::Lit;
use crate::structs::*;
use ompas_lisp::language::scheme_primitives::EVAL;
use ompas_lisp::structs::LValue;

pub fn translate_lvalue_to_expression_chronicle_r(
    exp: &LValue,
    sym_table: &mut SymTable,
) -> ExpressionChronicle {
    let mut ec = ExpressionChronicle::new(exp.clone(), sym_table);

    match exp {
        LValue::Symbol(_) => {
            ec.set_lit(Lit::LValue(vec![EVAL.into(), exp.clone()].into()));
        }
        LValue::List(l) => {
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

            let literal: Lit = literal.into();

            let expression_chronicle = Expression {
                interval: ec.get_interval().clone(),
                lit: literal.clone(),
            };

            ec.set_lit(literal);

            ec.add_subtask(expression_chronicle);
        }
        LValue::Nil
        | LValue::True
        | LValue::Number(_)
        | LValue::String(_)
        | LValue::Character(_) => {
            ec.set_lit(exp.into());
        }
        _ => {
            panic!()
        }
    }

    let val = ec.get_lit();

    ec.add_effect(Effect {
        interval: TransitionInterval::new(
            ec.get_interval().clone(),
            sym_table.declare_new_timepoint(),
        ),
        transition: Transition::new(ec.get_result().into(), val),
    });

    ec
}

pub fn translate_lvalue_to_chronicle(exp: &[LValue], sym_table: &mut SymTable) -> Chronicle {
    let mut chronicle = Chronicle::default();
    let interval = sym_table.declare_new_interval();
    chronicle.add_interval(&interval);
    let result = sym_table.declare_new_result();
    chronicle.add_var(&result);
    let f = sym_table.declare_new_object(Some(exp[0].to_string()));
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
