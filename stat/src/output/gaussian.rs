use crate::output::plot::Dat;
use crate::stat::config::ConfigName;
use crate::stat::problem::ProblemName;
use crate::stat::system::SystemRunData;
use crate::stat::{Field, Stat};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Write;
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Gaussian {
    // first one is reference
    pub problem: String,
    pub configs: Vec<String>,
    pub field: Field,
    pub output_dat_dir: PathBuf,
    pub output_tex: PathBuf,
}

pub struct GaussianOutput {
    pub dats: Vec<Dat>,
    pub tex: String,
    pub output_tex: PathBuf,
}

impl Gaussian {
    pub fn to_latex(&self, run: &SystemRunData) -> crate::output::gaussian::GaussianOutput {
        let problem_name: Vec<&str> = self.problem.split('_').collect();
        let problem_name = ProblemName {
            domain: problem_name[0].to_string(),
            difficulty: problem_name[1].into(),
        };

        let problem_run_data = run
            .inner
            .get(&problem_name)
            .unwrap_or_else(|| panic!("Missing problem {problem_name} in data."));

        let reference_config = self.configs.first().unwrap();
        let mut rc: Option<ConfigName> = None;

        let mut values: HashMap<ConfigName, Vec<f64>> = HashMap::new();

        for data in problem_run_data.inner.values() {
            let (ref_c, stat) = data
                .inner
                .iter()
                .find(|c| c.0.to_string().contains(reference_config))
                .unwrap();
            rc = Some(ref_c.clone());

            let instance_reference = stat
                .get_config_instance_stat()
                .stat_map
                .get(&self.field)
                .unwrap()
                .mean;

            'a: for (name, config_data) in &data.inner {
                let mut check = false;
                for c in &self.configs[1..] {
                    if name.to_string().contains(c) {
                        check = true;
                        break;
                    }
                }
                if !check {
                    continue 'a;
                }
                let entry = match values.get_mut(name) {
                    Some(e) => e,
                    None => {
                        values.insert(name.clone(), vec![]);
                        values.get_mut(name).unwrap()
                    }
                };

                config_data
                    .get_field_per_instance(&self.field)
                    .iter()
                    .for_each(|d| entry.push(d / instance_reference))
            }
        }

        let mut dats = vec![];

        let mut x_min: f64 = 1.0;
        let mut x_max: f64 = 0.0;
        for (config_name, values) in &values {
            let mut lines = vec![format!("{}", self.field)];
            let stat: Stat = values.as_slice().into();
            for x in values {
                lines.push(format!("{}", x));
                x_min = x_min.min(*x);
                x_max = x_max.max(*x);
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
            dats.push((dat, (stat.mean, stat.sd)));
        }
        let max_distance = f64::max(1.0 - x_min, x_max - 1.0) * 1.2;
        x_min = 1.0 - max_distance;
        x_max = 1.0 + max_distance;

        let x_label = format!(
            "{}/{}({})",
            self.field.to_latex(),
            self.field.to_latex(),
            rc.unwrap().format()
        );

        let mut tex = format!(
            "
\\begin{{tikzpicture}}
    \\begin{{axis}}[
              /pgf/number format/.cd,
                  use comma,
              height = 6cm,
              width = \\linewidth,
              ymajorgrids,
              ylabel=Frequency,
              xlabel={x_label},
              xmin = {x_min},
              xmax = {x_max},
              legend pos = north west,
              legend image post style={{scale=0.4}},
              legend style={{font = \\footnotesize}},
              ]"
        );

        for (dat, (mean, sd)) in &dats {
            write!(
                tex,
                "
        \\addplot+[
        hist,
        hist/bins=20
        ]
        table[
            y={}
        ]{{{}}};
        \\addlegendentry{{{}}}
        \\addplot[domain={{{x_min}:{x_max}}}]{{\\gauss{{{mean}}}{{{sd}}}}};
            ",
                self.field,
                dat.path.display(),
                dat.name,
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

        GaussianOutput {
            dats: dats.drain(..).map(|(dat, _)| dat).collect(),
            tex,
            output_tex: self.output_tex.clone(),
        }
    }
}
