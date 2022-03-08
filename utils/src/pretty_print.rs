use aries_planning::parsing::sexpr::{parse, SExpr, SList};

const BEGIN: &str = "begin";
const IF: &str = "if";
const LET: &str = "let";
const LAMBDA: &str = "lambda";
const LET_STAR: &str = "let*";

pub fn pretty_print(string: &str, indent: usize) -> String {
    let sexpr = parse(string).expect("could not parse str");
    pretty_print_sexpr(&sexpr, indent)
}

const TAB_SIZE: usize = 3;

fn pretty_print_slist(list: &SList, indent: usize) -> String {
    let mut str = '('.to_string();
    if list.iter().len() < 4 {
        for (i, element) in list.iter().enumerate() {
            if i > 0 {
                str.push(' ');
            }
            str.push_str(pretty_print_sexpr(element, indent + TAB_SIZE).as_str());
        }
    } else {
        for (i, element) in list.iter().enumerate() {
            match i {
                0 => {
                    str.push_str(pretty_print_sexpr(element, indent + TAB_SIZE).as_str());
                }
                1 => {
                    str.push(' ');
                    str.push_str(pretty_print_sexpr(element, indent + TAB_SIZE).as_str());
                }
                _ => str.push_str(
                    format!(
                        "\n{}{}",
                        " ".repeat(indent + TAB_SIZE),
                        pretty_print_sexpr(element, indent + TAB_SIZE)
                    )
                    .as_str(),
                ),
            }
        }
    }
    str.push(')');
    str
}

fn pretty_print_sexpr(sexpr: &SExpr, indent: usize) -> String {
    match sexpr {
        SExpr::Atom(a) => a.to_string(),
        SExpr::List(list) => {
            if let Ok(atom) = list.iter().pop_atom() {
                match atom.canonical_str() {
                    BEGIN => {
                        let indent = indent + TAB_SIZE;
                        let mut str = "(begin".to_string();
                        for (i, element) in list.iter().enumerate() {
                            if i > 0 {
                                str.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        pretty_print_sexpr(element, indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                )
                            }
                        }
                        str.push(')');
                        str
                    }
                    IF => {
                        let mut str = String::new();
                        let indent = indent + 4;
                        for (i, element) in list.iter().enumerate() {
                            match i {
                                0 => {
                                    str.push_str("(if");
                                }
                                1 => {
                                    str.push(' ');
                                    str.push_str(pretty_print_sexpr(element, indent).as_str());
                                }
                                _ => str.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        pretty_print_sexpr(element, indent)
                                    )
                                    .as_str(),
                                ),
                            }
                        }
                        str.push(')');
                        str
                    }
                    LAMBDA => {
                        let mut str = "(lambda ".to_string();
                        for (i, element) in list.iter().enumerate() {
                            match i {
                                0 => {}
                                1 => str.push_str(
                                    pretty_print_sexpr(element, indent + TAB_SIZE).as_str(),
                                ),
                                2 => str.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent + TAB_SIZE),
                                        pretty_print_sexpr(element, indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                ),
                                _ => unreachable!("todo"),
                            }
                        }
                        str.push(')');
                        str
                    }
                    LET | LET_STAR => {
                        let mut indent = indent;
                        let mut str = String::new();
                        match atom.canonical_str() {
                            LET => {
                                str.push_str("(let ");
                                indent += 5;
                            }
                            LET_STAR => {
                                str.push_str("(let* ");
                                indent +=6;
                            }
                            _ => unreachable!("The value of the atom has been checked before and should be let or let*.")

                        }
                        for (i, element) in list.iter().enumerate() {
                            match i {
                                1 => {
                                    if let Some(bindings) = element.as_list() {
                                        str.push('(');
                                        for (i, binding) in bindings.iter().enumerate() {
                                            if i == 0 {
                                                str.push_str(
                                                    pretty_print_sexpr(
                                                        binding,
                                                        indent + 1 + TAB_SIZE,
                                                    )
                                                    .as_str(),
                                                );
                                            } else {
                                                str.push_str(
                                                    format!(
                                                        "\n{}{}",
                                                        " ".repeat(indent + 1),
                                                        pretty_print_sexpr(
                                                            binding,
                                                            indent + 1 + TAB_SIZE
                                                        )
                                                    )
                                                    .as_str(),
                                                );
                                            }
                                        }
                                        str.push(')')
                                    } else {
                                        panic!("should be a list")
                                    }
                                }
                                2 => str.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        pretty_print_sexpr(element, indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                ),
                                _ => {}
                            }
                        }

                        str.push(')');
                        str
                    }
                    _ => pretty_print_slist(list, indent),
                }
            } else {
                pretty_print_slist(list, indent)
            }
        }
    }
}
