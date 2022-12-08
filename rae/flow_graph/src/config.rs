use std::path::PathBuf;
use yaml_rust::Yaml;

const INPUT_PATH: &str = "input-path";
const OUTPUT_PATH: &str = "output-path";
const PROBLEMS: &str = "problems";

#[derive(Default, Debug, Clone)]
pub struct GraphConvertConfig {
    pub input_path: Option<PathBuf>,
    pub output_path: Option<PathBuf>,
    pub problems: Vec<PathBuf>,
}

impl TryFrom<&Yaml> for GraphConvertConfig {
    type Error = String;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        let temp_problems = value[PROBLEMS]
            .as_vec()
            .ok_or("problems is not an array.")?;
        let mut problems = vec![];
        for p in temp_problems {
            problems.push(
                p.as_str()
                    .ok_or_else(|| panic!("Not a str"))
                    .unwrap()
                    .into(),
            )
        }

        Ok(Self {
            input_path: value[INPUT_PATH].as_str().map(PathBuf::from),
            output_path: value[OUTPUT_PATH].as_str().map(PathBuf::from),
            problems,
        })
    }
}
