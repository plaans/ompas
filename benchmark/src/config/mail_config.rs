use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

#[derive(Serialize, Deserialize)]
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
