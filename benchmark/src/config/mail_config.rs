use crate::config::ConfigError;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use yaml_rust::Yaml;

pub struct MailConfig {
    pub from: String,
    pub to: String,
    pub smtp: String,
    pub password: String,
}

impl Display for MailConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "mail:\n\
           \t-from: {}\n\
            \t-to: {}\n\
            \t-smtp: {}\n\
            \t-password: *****
        ",
            self.from, self.to, self.smtp
        )
    }
}

impl TryFrom<&Yaml> for MailConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        Ok(Self {
            from: value["from"]
                .as_str()
                .ok_or("from should be a string")?
                .to_string(),
            to: value["to"]
                .as_str()
                .ok_or("to should be a string")?
                .to_string(),
            smtp: value["smtp"]
                .as_str()
                .ok_or("smtp should be a string")?
                .to_string(),
            password: value["password"]
                .as_str()
                .ok_or("password should be a string")?
                .to_string(),
        })
    }
}
