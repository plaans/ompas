use crate::stat::problem::ProblemName;
use crate::stat::system::SystemRunData;
use crate::stat::Field;
use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Bar {
    instance: String,
    pub fields: Vec<Field>,
    pub configs: Vec<String>,
    output_dat: PathBuf,
    output_tex: PathBuf,
}

pub struct BarOutput {
    pub dat: String,
    pub tex: String,
    pub output_dat: PathBuf,
    pub output_tex: PathBuf,
}

impl Bar {
    pub fn to_latex(&self, run: &SystemRunData) -> BarOutput {
        let instance_name: Vec<&str> = self.instance.split("_").collect();
        let problem_name = ProblemName {
            domain: instance_name[0].to_string(),
            difficulty: instance_name[1].into(),
        };
        let instance_name = instance_name[2].to_string();

        let instance_run_data = run
            .inner
            .get(&problem_name)
            .unwrap()
            .inner
            .get(&instance_name)
            .unwrap();
        let stat = instance_run_data.get_stat();
        let mut dat = "Config".to_string();
        for field in &self.fields {
            write!(dat, " {} {}E", field, field).unwrap();
        }
        dat.push('\n');
        let mut x_tick = "{".to_string();
        let mut x_tick_labels = "{".to_string();
        let mut y_min: f64 = 0.0;
        let mut y_max: f64 = 0.0;
        for (i, (config_name, config_stat)) in stat
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
            if i > 0 {
                x_tick.push(',');
                x_tick_labels.push(',');
            }
            write!(x_tick, "{}", i).unwrap();
            write!(x_tick_labels, "{}", config_name.format()).unwrap();
            write!(dat, "{}", i).unwrap();
            for field in &self.fields {
                if let Some(stat) = config_stat.get(&field) {
                    write!(dat, " {} {}", stat.mean, stat.se).unwrap();
                    y_min = y_min.min(stat.mean - stat.se);
                    y_max = y_max.max(stat.mean + stat.se);
                } else {
                    write!(dat, " 0 0").unwrap();
                }
            }
            dat.push('\n');
        }
        x_tick.push('}');
        x_tick_labels.push('}');
        y_min *= 1.1;
        y_max *= 1.2;

        let x_label = "Configuration";
        let y_label = "Time (s)";

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
              ybar=0pt,
              bar width=12pt,
              enlarge x limits = 0.3,
              point meta=explicit symbolic,
              scatter/position=absolute,
              every node near coord/.style={{
                      at={{(\\pgfkeysvalueof{{/data point/x}},1.8)}},
                      anchor=south,
                  }},
              bar shift=0pt,
              xtick={x_tick},
              xticklabels={x_tick_labels},
              x tick label style={{rotate=45,anchor=east}},
              legend cell align = {{left}},
              legend pos = north west,
              legend image post style={{scale=0.4}},
              legend style={{font = \\footnotesize}},
              ]"
        );

        for field in &self.fields {
            write!(
                tex,
                "
            \\addplot+[
            bar shift = 0pt,
            error bars/.cd,
            y dir=both,
            y explicit
            ]
            table[
                x=Config,
                y={},
                y error={}E]{{{}}};
            \\addlegendentry{{{}}}
                ",
                field,
                field,
                self.output_dat.display(),
                field.to_latex(),
            )
            .unwrap();
        }

        write!(
            tex,
            "
    \\end{{axis}}     
\\end{{tikzpicture}}",
        )
        .unwrap();

        BarOutput {
            dat,
            tex,
            output_dat: self.output_dat.clone(),
            output_tex: self.output_tex.clone(),
        }
    }
}
