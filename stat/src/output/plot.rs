use crate::stat::planning::PlanningField;
use crate::stat::problem::ProblemName;
use crate::stat::system::SystemRunData;
use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Plot {
    pub instance: String,
    pub configs: Vec<String>,
    pub fields: Vec<PlanningField>,
    pub output_dat_dir: PathBuf,
    pub output_tex: PathBuf,
}

pub struct PlotOutput {
    pub dats: Vec<Dat>,
    pub tex: String,
    pub output_tex: PathBuf,
}

pub struct Dat {
    pub content: String,
    pub name: String,
    pub path: PathBuf,
}

impl Plot {
    pub fn to_latex(&self, run: &SystemRunData) -> PlotOutput {
        let instance_name: Vec<&str> = self.instance.split("_").collect();
        let problem_name = ProblemName {
            domain: instance_name[0].to_string(),
            difficulty: instance_name[1].into(),
        };
        let instance_name = instance_name[2].to_string();

        let instance_run_data = run
            .inner
            .get(&problem_name)
            .unwrap_or_else(|| panic!("Missing problem {problem_name} in data."))
            .inner
            .get(&instance_name)
            .unwrap_or_else(|| panic!("Missing instance {instance_name} in data."));
        let stat = instance_run_data.get_planning_stat();

        let mut instance = 0;

        let mut dats = vec![];

        let mut y_min: f64 = 0.0;
        let mut y_max: f64 = 0.0;
        for (_, (config_name, config_stat)) in stat
            .inner
            .iter()
            .filter(|(name, _)| {
                let name = name.to_string();
                for c in &self.configs {
                    if name.contains(c) {
                        return true;
                    }
                }
                false
            })
            .enumerate()
        {
            let mut lines = vec!["Instance".to_string()];
            for field in &self.fields {
                lines[0].push_str(format!(" {} {}E", field, field).as_str());
                if let Some(stat) = config_stat.get(&field) {
                    instance = instance.max(stat.len());
                    for (i, stat) in stat.iter().enumerate() {
                        let i = i + 1;
                        if lines.len() < i + 1 {
                            lines.push(format!("{}", i - 1));
                        }
                        write!(lines[i], " {} {}", stat.mean, stat.se).unwrap();
                        y_min = y_min.min(stat.mean - stat.se);
                        y_max = y_max.max(stat.mean + stat.se);
                    }
                }
            }
            let mut dat = String::new();
            for line in lines {
                writeln!(dat, "{}", line).unwrap();
            }
            let mut path = self.output_dat_dir.clone();
            path.push(format!("{}.dat", config_name));
            let dat = Dat {
                content: dat,
                name: config_name.format(),
                path,
            };
            dats.push(dat)
        }
        y_min *= 1.1;
        y_max *= 1.1;
        let mut x_tick = '{'.to_string();
        let n = instance / 10;
        let n2 = n / 5 + 1;
        for i in 0..11 {
            if i > 0 {
                x_tick.push(',');
            }
            let i = i * n2 * 5;
            write!(x_tick, "{}", i).unwrap();
        }
        x_tick.push('}');

        let x_label = "Instance";
        let y_label = format!("{} ({})", self.fields[0].to_latex(), self.fields[0].unit());

        let mut tex = format!(
            "\
\\begin{{tikzpicture}}
    \\begin{{axis}}[
              /pgf/number format/.cd,
                  use comma,
              height = 6cm,
              width = \\linewidth,
              ymajorgrids,
              ylabel={y_label},
              xlabel={x_label},
              ymin = {y_min},
              ymax= {y_max},
              bar width=12pt,
              enlarge x limits = 0.3,
              point meta=explicit symbolic,
              scatter/position=absolute,
              every node near coord/.style={{
                      at={{(\\pgfkeysvalueof{{/data point/x}},1.8)}},
                      anchor=south,
                  }},
              xtick={x_tick},
              xticklabels={x_tick},
              x tick label style={{rotate=45,anchor=east}},
              legend cell align = {{left}},
              legend pos = north west,
              legend image post style={{scale=0.4}},
              legend style={{font = \\footnotesize}},
              ]"
        );

        for field in &self.fields {
            for dat in &dats {
                write!(
                    tex,
                    "
            \\addplot+[
                error bars/.cd,
                y explicit,
                y dir=both,
                ]
            table[
                x=Instance,
                y={},
                y error={}E]{{{}}};
            \\addlegendentry{{{}}}
                ",
                    field,
                    field,
                    dat.path.display(),
                    dat.name,
                )
                .unwrap();
            }
        }

        write!(
            tex,
            "
    \\end{{axis}}     
\\end{{tikzpicture}}",
        )
        .unwrap();

        PlotOutput {
            dats,
            tex,
            output_tex: self.output_tex.clone(),
        }
    }
}
