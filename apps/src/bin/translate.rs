use ompas_lisp::core::{parse, LEnv};
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue};
use ompas_planning::algo::{
    translate_lvalue_to_chronicle, translate_lvalue_to_expression_chronicle_r,
};
use ompas_planning::structs::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[tokio::main]
async fn main() -> Result<(), LError> {
    println!("translate binary");
    let mut rl = Editor::<()>::new();
    if rl.load_history("history_translate.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(string) => {
                rl.add_history_entry(string.clone());
                let _ = translate_2(&string).await?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history_translate.txt").unwrap();

    Ok(())
}

async fn translate_2(exp: &str) -> Result<ExpressionChronicle, LError> {
    let (mut env, mut ctxs) = LEnv::root().await;
    let lv = parse(exp, &mut env, &mut ctxs).await?;

    let mut symbol_table = SymTable::default();

    let chronicle = translate_lvalue_to_expression_chronicle_r(&lv, &mut symbol_table);
    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(chronicle)
}

async fn translate(exp: &str) -> Result<Chronicle, LError> {
    let (mut env, mut ctxs) = LEnv::root().await;
    let lv = parse(exp, &mut env, &mut ctxs).await?;

    let slice = if let LValue::List(l) = &lv {
        l.as_slice()
    } else {
        return Err(SpecialError(
            "MAIN_TRANSLATE",
            "exp should be a list".to_string(),
        ));
    };

    let mut symbol_table = SymTable::default();

    let chronicle = translate_lvalue_to_chronicle(slice, &mut symbol_table);
    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(chronicle)
}

#[tokio::test]
async fn test() -> Result<(), LError> {
    let exp = "(test 1 2 3 4)";
    let (mut env, mut ctxs) = LEnv::root().await;
    let lv = parse(exp, &mut env, &mut ctxs).await?;

    let slice = if let LValue::List(l) = &lv {
        l.as_slice()
    } else {
        return Err(SpecialError(
            "MAIN_TRANSLATE",
            "exp should be a list".to_string(),
        ));
    };

    let mut symbol_table = SymTable::default();

    let chronicle = translate_lvalue_to_chronicle(slice, &mut symbol_table);

    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(())
}
