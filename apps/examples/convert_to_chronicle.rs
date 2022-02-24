use ompas_lisp::core::parse;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError;

use ompas_rae::planning::conversion::processing::translate_lvalue_to_expression_chronicle;
use ompas_rae::planning::conversion::translate_lvalue_to_chronicle;
use ompas_rae::planning::structs::chronicle::{Chronicle, ExpressionChronicle};
use ompas_rae::planning::structs::symbol_table::SymTable;
use ompas_rae::planning::structs::traits::FormatWithSymTable;
use ompas_rae::planning::structs::ConversionContext;
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
    let mut env = LEnv::root().await;
    let lv = parse(exp, &mut env).await?;

    let mut symbol_table = SymTable::default();

    let context = ConversionContext::default();

    let chronicle = translate_lvalue_to_expression_chronicle(&lv, &context, &mut symbol_table)?;
    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(chronicle)
}

#[allow(dead_code)]
async fn translate(exp: &str) -> Result<Chronicle, LError> {
    let mut env = LEnv::root().await;
    let lv = parse(exp, &mut env).await?;

    let context = ConversionContext::default();

    let mut symbol_table = SymTable::default();

    let chronicle = translate_lvalue_to_chronicle(&lv, &context, &mut symbol_table)?;
    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(chronicle)
}

#[tokio::test]
async fn test() -> Result<(), LError> {
    let exp = "(test 1 2 3 4)";
    let mut env = LEnv::root().await;
    let lv = parse(exp, &mut env).await?;

    let mut symbol_table = SymTable::default();

    let chronicle =
        translate_lvalue_to_chronicle(&lv, &ConversionContext::default(), &mut symbol_table)?;

    println!("{}", chronicle.format_with_sym_table(&symbol_table));
    Ok(())
}