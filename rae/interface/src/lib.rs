use crate::platform_interface::atom::Kind;
use crate::platform_interface::{
    atom, command_response, Atom, CommandAccepted, CommandCancelled, CommandProgress,
    CommandRejected, CommandResponse, CommandResult, Expression,
};
use sompas_structs::lvalues::LValueS;
use std::fmt::{Display, Formatter};

pub mod platform;
pub mod platform_interface;

pub const DEFAULT_PLATFORM_SERVICE_IP: &str = "127.0.0.1";
pub const DEFAULT_PLATFROM_SERVICE_PORT: u16 = 8257;

const TOKIO_CHANNEL_SIZE: usize = 100;

impl TryFrom<LValueS> for Atom {
    type Error = ();

    fn try_from(value: LValueS) -> Result<Self, Self::Error> {
        Ok(match value {
            LValueS::Symbol(s) => Atom {
                kind: Some(Kind::Symbol(s)),
            },
            LValueS::Int(i) => Atom {
                kind: Some(Kind::Int(i)),
            },
            LValueS::Float(f) => Atom {
                kind: Some(Kind::Float(f)),
            },
            LValueS::Bool(b) => Atom {
                kind: Some(Kind::Boolean(b)),
            },
            _ => return Err(()),
        })
    }
}

impl TryFrom<LValueS> for Expression {
    type Error = ();

    fn try_from(mut value: LValueS) -> Result<Self, Self::Error> {
        Ok(match value {
            LValueS::Symbol(s) => Self {
                atom: Some(Atom {
                    kind: Some(Kind::Symbol(s)),
                }),
                list: vec![],
            },
            LValueS::Int(i) => Self {
                atom: Some(Atom {
                    kind: Some(Kind::Int(i)),
                }),
                list: vec![],
            },
            LValueS::Float(f) => Self {
                atom: Some(Atom {
                    kind: Some(Kind::Float(f)),
                }),
                list: vec![],
            },
            LValueS::Bool(b) => Self {
                atom: Some(Atom {
                    kind: Some(Kind::Boolean(b)),
                }),
                list: vec![],
            },
            LValueS::List(mut vec) => {
                let mut list = vec![];
                for lvs in vec.drain(..) {
                    list.push(Expression::try_from(lvs)?);
                }
                Self { atom: None, list }
            }
            LValueS::Map(_) => return Err(()),
        })
    }
}

impl TryFrom<&Atom> for LValueS {
    type Error = ();

    fn try_from(value: &Atom) -> Result<Self, Self::Error> {
        let atom = match &value.kind {
            None => return Err(()),
            Some(atom) => atom,
        };
        Ok(match atom {
            atom::Kind::Symbol(s) => s.clone().into(),
            atom::Kind::Int(i) => (*i).into(),
            atom::Kind::Float(f) => (*f).into(),
            atom::Kind::Boolean(b) => (*b).into(),
        })
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            None => write!(f, ""),
            Some(Kind::Float(fl)) => write!(f, "{}", fl),
            Some(Kind::Int(i)) => write!(f, "{}", i),
            Some(Kind::Symbol(s)) => write!(f, "{}", s),
            Some(Kind::Boolean(b)) => write!(f, "{}", b),
        }
    }
}

impl TryFrom<&Expression> for LValueS {
    type Error = ();

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        if let Some(a) = &value.atom {
            Ok(a.try_into()?)
        } else {
            let mut vec: Vec<LValueS> = vec![];
            for e in &value.list {
                vec.push(LValueS::try_from(e)?);
            }
            Ok(vec.into())
        }
    }
}

impl From<CommandAccepted> for CommandResponse {
    fn from(ca: CommandAccepted) -> Self {
        Self {
            response: Some(command_response::Response::Accepted(ca)),
        }
    }
}

impl From<CommandRejected> for CommandResponse {
    fn from(cr: CommandRejected) -> Self {
        Self {
            response: Some(command_response::Response::Rejected(cr)),
        }
    }
}

impl From<CommandProgress> for CommandResponse {
    fn from(cp: CommandProgress) -> Self {
        Self {
            response: Some(command_response::Response::Progress(cp)),
        }
    }
}

impl From<CommandResult> for CommandResponse {
    fn from(cr: CommandResult) -> Self {
        Self {
            response: Some(command_response::Response::Result(cr)),
        }
    }
}

impl From<CommandCancelled> for CommandResponse {
    fn from(cc: CommandCancelled) -> Self {
        Self {
            response: Some(command_response::Response::Cancelled(cc)),
        }
    }
}
