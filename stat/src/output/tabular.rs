use crate::stat::config::ConfigProblemStat;
use crate::stat::system::SystemStat;
use crate::stat::Field;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Write};
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Tabular {
    pub csv_output: Option<PathBuf>,
    pub latex_output: Option<PathBuf>,
    pub fields: Vec<Field>,
    pub configs: Vec<String>,
    pub problems: Vec<String>,
    pub no_problem: Option<bool>,
    pub no_complexity: Option<bool>,
}

pub struct TabularOutput {
    lines: Vec<Vec<Cell>>,
}

impl TabularOutput {
    pub fn to_csv(&self) -> String {
        let mut string = String::new();
        let mut multi_line: Option<(usize, usize)> = None;
        for (line, cells) in self.lines.iter().enumerate() {
            let mut c_i = 0;
            for cell in cells {
                if let Some((c, l)) = multi_line {
                    if c_i == c {
                        string.push(';');
                    }
                    if line > l {
                        multi_line = None;
                    }
                }
                if cell.row > 1 {
                    multi_line = Some((c_i, line + cell.row - 1))
                }
                if c_i != 0 {
                    string.push(';');
                }
                write!(string, "{}", cell.info).unwrap();
                write!(string, "{}", ";".repeat(cell.column - 1)).unwrap();
                c_i += cell.column;
            }
            string.push('\n');
        }

        string
    }

    pub fn to_latex(&self) -> String {
        let mut columns = String::new();
        for cell in &self.lines[0] {
            columns.push_str("|c|");
            for _ in 0..cell.column - 1 {
                columns.push_str("c|");
            }
        }

        let mut string = format!("\\begin{{tabular}}{{{}}}\n\\hline\n", columns);
        let mut multi_row: Option<usize> = None;
        for (l_i, line) in self.lines.iter().enumerate() {
            if let Some(row_span) = multi_row {
                string.push('&');
                if row_span == l_i {
                    multi_row = None;
                }
            }
            for (i, cell) in line.iter().enumerate() {
                if i != 0 {
                    string.push('&');
                }
                let mut cell_info = cell.info.to_string();
                if cell.column > 1 {
                    cell_info = format!("\\multicolumn{{{}}}{{|c|}}{{{}}}", cell.column, cell_info);
                }
                if cell.row > 1 {
                    cell_info = format!("\\multirow{{{}}}{{*}}{{{}}}", cell.row, cell_info);
                    multi_row = Some(l_i + cell.row - 1);
                }
                write!(string, "{}", cell_info).unwrap();
            }
            if multi_row.is_some() {
                writeln!(string, "\\\\").unwrap();
            } else {
                writeln!(string, "\\\\\\hline").unwrap();
            }
        }

        writeln!(string, "\\end{{tabular}}").unwrap();
        string
    }
}

impl Tabular {
    pub fn new_tabular_output(&self, value: &SystemStat) -> TabularOutput {
        // let mut configs: HashMap<&ConfigName, HashMap<&ProblemName, &ConfigProblemStat>> =
        //     Default::default();
        // let mut config_order: OrdMap<usize, &ConfigName> = Default::default();
        // for (name, problem) in &value.problem_stats {
        //     for (i, filter_config) in self.configs.iter().enumerate() {
        //         for (config_name, config) in &problem.inner {
        //             if filter_config == &config_name.to_string() {
        //                 match configs.entry(config_name) {
        //                     Entry::Occupied(mut o) => {
        //                         o.get_mut().insert(name, config);
        //                     }
        //                     Entry::Vacant(v) => {
        //                         config_order.insert(i, config_name);
        //                         let mut map = HashMap::default();
        //                         map.insert(name, config);
        //                         v.insert(map);
        //                     }
        //                 };
        //             }
        //         }
        //     }
        // }

        let mut lines = vec![];
        for filter_problem in &self.problems {
            'loop_problem: for (name, problem) in &value.problem_stats {
                //println!("problem: {}", name);
                if !name
                    .to_string()
                    .to_ascii_lowercase()
                    .contains(filter_problem)
                {
                    //println!("skip {}", name);
                    continue 'loop_problem;
                }
                if !matches!(self.no_problem, Some(true)) {
                    lines.push(vec![
                        Cell::double("Problem".to_string()),
                        Cell {
                            info: name.domain.to_string(),
                            column: self.fields.len(),
                            row: 1,
                            pre_sep: Some('|'),
                            post_sep: Some('|'),
                        },
                    ]);
                }
                if !matches!(self.no_complexity, Some(true)) {
                    lines.push(vec![
                        Cell::double("Complexity".to_string()),
                        Cell {
                            info: name.difficulty.to_string(),
                            column: self.fields.len(),
                            row: 1,
                            pre_sep: Some('|'),
                            post_sep: Some('|'),
                        },
                    ]);
                }
                let mut header = vec![Cell::double("Config".to_string())];
                header.append(&mut ConfigProblemStat::header(&self.fields));
                let header_len = header.len() - 1;
                let mut empty_info = vec![Cell::start("ND".to_string()); header_len - 1];
                empty_info.push(Cell::double("ND".to_string()));
                lines.push(header);
                for filter_config in &self.configs {
                    'loop_config: for (config_name, config) in &problem.inner {
                        if !config_name.to_string().contains(filter_config) {
                            continue 'loop_config;
                        }

                        let mut line = vec![Cell::double(config_name.format())];
                        line.append(&mut config.to_formatted(&self.fields));
                        lines.push(line);
                    }
                }
            }
        }

        // let mut first_line = vec![Cell {
        //     column: 2,
        //     row: 1,
        //     pre_sep: Some('|'),
        //     post_sep: Some('|'),
        //     info: "".to_string(),
        // }];
        // let mut second_line = vec![
        //     Cell::start("Problem".to_string()),
        //     Cell::double("Complexity".to_string()),
        // ];
        // let header = ConfigProblemStat::header(&self.fields);
        // let header_len = header.len();
        // let mut empty_info = vec![Cell::start("ND".to_string()); header_len - 1];
        // empty_info.push(Cell::double("ND".to_string()));
        // for (_, config_name) in &config_order {
        //     first_line.push(Cell {
        //         info: config_name.format(),
        //         column: header_len,
        //         row: 1,
        //         pre_sep: Some('|'),
        //         post_sep: Some('|'),
        //     });
        //     second_line.append(&mut header.clone())
        // }
        // lines.push(first_line);
        // lines.push(second_line);
        //
        // let mut names: Vec<_> = value.problem_stats.keys().collect();
        // names.sort();
        //
        // let mut same_domain: Option<usize> = None;
        // for (l_i, problem_name) in names.iter().enumerate() {
        //     let mut line = vec![];
        //     if let Some(max_span) = same_domain {
        //         if max_span < l_i {
        //             same_domain = None;
        //         }
        //     }
        //     if same_domain.is_none() {
        //         let mut n = 1;
        //         while l_i + n < names.len() {
        //             if names[l_i + n].domain == problem_name.domain {
        //                 n += 1;
        //             } else {
        //                 break;
        //             }
        //         }
        //         line.push(Cell {
        //             info: problem_name.domain.to_string(),
        //             column: 1,
        //             row: n,
        //             pre_sep: Some('|'),
        //             post_sep: None,
        //         });
        //         let max_span = l_i + n - 1;
        //         /*println!(
        //             "number of line for {}: {}, max_span = {}",
        //             problem_name.domain, n, max_span
        //         );*/
        //         same_domain = Some(max_span);
        //     }
        //     line.push(Cell::double(problem_name.difficulty.to_string()));
        //     for (_, config) in &config_order {
        //         let config = configs.get(config).unwrap();
        //         match config.get(problem_name) {
        //             None => {
        //                 line.append(&mut empty_info.clone());
        //             }
        //             Some(config) => line.append(&mut config.to_formatted(&self.fields)),
        //         }
        //     }
        //     lines.push(line);
        // }
        TabularOutput { lines }
    }
}

impl Display for TabularOutput {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // let mut column_span = None;
        let mut line_span: Option<(usize, usize, &Cell)> = None;

        let mut column_size: Vec<usize> = vec![
            0;
            self.lines[0]
                .iter()
                .fold(0, |prev, cell| prev + cell.column)
        ];
        for cells in &self.lines {
            let mut c_i = 0;
            for cell in cells {
                column_size[c_i] = column_size[c_i].max(cell.info.len());
                c_i += cell.column;
            }
        }

        let write_cell = |cell: &Cell, i: &mut usize, f: &mut Formatter<'_>| -> std::fmt::Result {
            if cell.column == 1 {
                write!(f, "{}", cell.format(column_size[*i]))?;
                *i += 1;
            } else {
                write!(
                    f,
                    "{}",
                    Cell {
                        info: cell.info.to_string(),
                        column: 1,
                        row: 1,
                        pre_sep: cell.pre_sep,
                        post_sep: None,
                    }
                    .format(column_size[*i])
                )?;
                *i += 1;
                for _ in 1..cell.column - 1 {
                    write!(
                        f,
                        "{}",
                        Cell {
                            info: "".to_string(),
                            column: 1,
                            row: 1,
                            pre_sep: Some(' '),
                            post_sep: None,
                        }
                        .format(column_size[*i])
                    )?;
                    *i += 1;
                }
                write!(
                    f,
                    "{}",
                    Cell {
                        info: "".to_string(),
                        column: 1,
                        row: 1,
                        pre_sep: Some(' '),
                        post_sep: cell.post_sep.clone()
                    }
                    .format(column_size[*i])
                )?;
                *i += 1;
            }
            Ok(())
        };

        for (line, cells) in self.lines.iter().enumerate() {
            let mut c_i = 0;
            for cell in cells {
                if let Some((c, l, cell)) = line_span {
                    if c == c_i {
                        write_cell(
                            &Cell {
                                info: "".to_string(),
                                column: cell.column,
                                row: cell.row,
                                pre_sep: cell.pre_sep,
                                post_sep: cell.post_sep,
                            },
                            &mut c_i,
                            f,
                        )?;
                    }
                    if line > l {
                        line_span = None;
                    }
                }
                if cell.row > 1 {
                    let max_line_span = line + cell.row - 1;
                    println!("max_line_span = {}", max_line_span);
                    line_span = Some((c_i, line + max_line_span, cell));
                }
                write_cell(cell, &mut c_i, f)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Cell {
    info: String,
    column: usize,
    row: usize,
    pre_sep: Option<char>,
    post_sep: Option<char>,
}

impl Cell {
    pub fn empty() -> Self {
        Self {
            pre_sep: None,
            column: 1,
            row: 1,
            post_sep: None,
            info: "".to_string(),
        }
    }

    pub fn end(info: String) -> Self {
        Self {
            pre_sep: None,
            column: 1,
            row: 1,
            post_sep: Some('|'),
            info,
        }
    }

    pub fn start(info: String) -> Self {
        Self {
            pre_sep: Some('|'),
            column: 1,
            row: 1,
            post_sep: None,
            info,
        }
    }

    pub fn double(info: String) -> Self {
        Self {
            pre_sep: Some('|'),
            column: 1,
            row: 1,
            post_sep: Some('|'),
            info,
        }
    }

    pub fn format(&self, cell_size: usize) -> String {
        let mut string = "".to_string();
        if let Some(pre) = self.pre_sep {
            string.push(pre)
        }

        write!(string, "{:^cell_size$}", self.info).unwrap();

        if let Some(post) = self.post_sep {
            string.push(post)
        }

        string
    }
}
