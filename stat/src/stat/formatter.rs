use crate::stat::config::{ConfigName, ConfigProblemStat};
use crate::stat::problem::ProblemName;
use crate::stat::system::SystemStat;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};

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

pub struct SystemStatFormatter {
    lines: Vec<Vec<Cell>>,
}

impl SystemStatFormatter {
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

        let mut string = format!("\\begin{{tabular}}{{|{}}}\n\\hline", columns);
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

impl From<&SystemStat> for SystemStatFormatter {
    fn from(value: &SystemStat) -> Self {
        let mut configs: HashMap<&ConfigName, HashMap<&ProblemName, &ConfigProblemStat>> =
            Default::default();
        let mut config_order: Vec<&ConfigName> = Default::default();
        for (name, problem) in &value.problem_stats {
            for (config_name, config) in &problem.inner {
                match configs.entry(config_name) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().insert(name, config);
                    }
                    Entry::Vacant(v) => {
                        config_order.push(config_name);
                        let mut map = HashMap::default();
                        map.insert(name, config);
                        v.insert(map);
                    }
                };
            }
        }

        let mut lines = vec![];
        let mut first_line = vec![Cell {
            column: 2,
            row: 1,
            pre_sep: Some('|'),
            post_sep: Some('|'),
            info: "".to_string(),
        }];
        let mut second_line = vec![
            Cell::start("Problem".to_string()),
            Cell::double("Difficulty".to_string()),
        ];
        let header = ConfigProblemStat::header(&value.config.fields);
        let header_len = header.len();
        let mut empty_info = vec![Cell::start("ND".to_string()); header_len - 1];
        empty_info.push(Cell::double("".to_string()));
        for config_name in &config_order {
            first_line.push(Cell {
                info: config_name.format(),
                column: header_len,
                row: 1,
                pre_sep: Some('|'),
                post_sep: Some('|'),
            });
            second_line.append(&mut header.clone())
        }
        lines.push(first_line);
        lines.push(second_line);

        let mut names: Vec<_> = value.problem_stats.keys().collect();
        names.sort();

        let mut same_domain: Option<usize> = None;
        for (l_i, problem_name) in names.iter().enumerate() {
            let mut line = vec![];
            if let Some(span_domain) = same_domain {
                if span_domain < l_i {
                    same_domain = None;
                }
            }
            if same_domain.is_none() {
                let mut n = 1;
                while l_i + n < names.len() {
                    if names[l_i + n].domain == problem_name.domain {
                        n += 1;
                    } else {
                        break;
                    }
                }
                line.push(Cell {
                    info: problem_name.domain.to_string(),
                    column: 1,
                    row: n,
                    pre_sep: Some('|'),
                    post_sep: None,
                });
                same_domain = Some(l_i + n - 1);
            }
            line.push(Cell::double(problem_name.difficulty.to_string()));
            for config in &config_order {
                let config = configs.get(config).unwrap();
                match config.get(problem_name) {
                    None => {
                        line.append(&mut empty_info.clone());
                    }
                    Some(config) => line.append(&mut config.to_formatted(&value.config.fields)),
                }
            }
            lines.push(line);
        }
        SystemStatFormatter { lines }
    }
}

impl Display for SystemStatFormatter {
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
                );
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
            let mut i = 0;
            for cell in cells {
                if let Some((c, l, cell)) = line_span {
                    if c == i {
                        write_cell(
                            &Cell {
                                info: "".to_string(),
                                column: cell.column,
                                row: cell.row,
                                pre_sep: cell.pre_sep,
                                post_sep: cell.post_sep,
                            },
                            &mut i,
                            f,
                        )?;
                    }
                    if line > l {
                        line_span = None;
                    }
                }
                if cell.row > 1 {
                    line_span = Some((i, line + cell.row - 1, cell));
                }
                write_cell(cell, &mut i, f)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}
