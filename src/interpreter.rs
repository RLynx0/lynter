use std::rc::Rc;

use thiserror::Error;

use crate::lambda::Exp;

use self::{
    bimap::BiMap,
    parser::{parse, Parsed},
};

mod bimap;
mod parser;

pub enum Feedback<'a> {
    Bound {
        id: Rc<str>,
        exp: Exp<Rc<str>>,
    },
    Output {
        exp: Exp<Rc<str>>,
        matches: Option<&'a Vec<String>>,
    },

    NoOp,
    Comment(Rc<str>),
    Help,
    Quit,
    ClearScreen,
    List(Vec<(Rc<str>, Exp<Rc<str>>)>),

    Jerma,
    Load(Rc<str>),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("You requested an Error? ;)")]
    Requested,

    #[error("Unknown Command: '{0}'")]
    CommandUnknown(Rc<str>),

    #[error("Expected a command")]
    CommandEmpty,

    #[error("Symbol '{0}' is already bound")]
    AlreadyExists(Rc<str>),

    #[error("Symbol '{0}' is not bound")]
    SymbolUnbound(Rc<str>),

    #[error("Parse Error: {0}")]
    ParseErr(#[from] parser::Error),

    #[error("Command '{0}' expected an argument")]
    CommandExpectedArg(Rc<str>),
}

pub struct Interpreter {
    memory: BiMap<String, Exp<Rc<str>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            memory: BiMap::new(),
        }
    }

    pub fn interpret(&mut self, input: &str) -> Result<Feedback, Error> {
        match input {
            "" => Ok(Feedback::NoOp),
            c if c.starts_with(";;") => Ok(Feedback::Comment(Rc::from(c))),
            c if c.starts_with(':') => self.command(&c[1..]),
            _ => match parse(input) {
                Ok(Parsed::Bind { id, exp }) => match self.memory.get_value(&id) {
                    Some(_) => Err(Error::AlreadyExists(id.into())),
                    None => {
                        let exp = exp.deep_reduce(|s| {
                            self.memory
                                .get_value(&s.to_string())
                                .ok_or(Error::SymbolUnbound(s.clone()))
                                .map(|exp| exp.clone())
                        })?;
                        self.memory.insert(id.clone(), exp.clone());
                        Ok(Feedback::Bound { id: id.into(), exp })
                    }
                },
                Ok(Parsed::Pure(exp)) => {
                    let exp = exp.deep_reduce(|s| {
                        self.memory
                            .get_value(&s.to_string())
                            .ok_or(Error::SymbolUnbound(s.clone()))
                            .map(|exp| exp.clone())
                    })?;
                    Ok(Feedback::Output {
                        exp: exp.clone(),
                        matches: self.memory.get_keys(&exp),
                    })
                }
                Err(e) => Err(Error::ParseErr(e)),
            },
        }
    }

    fn command(&self, command: &str) -> Result<Feedback, Error> {
        let mut words = command.split_whitespace();
        match words.next().ok_or(Error::CommandEmpty)? {
            "q" | "quit" => Ok(Feedback::Quit),
            "?" | "help" => Ok(Feedback::Help),
            "c" | "clear" => Ok(Feedback::ClearScreen),
            "l" | "load" => Ok(Feedback::Load(Rc::from(
                words
                    .next()
                    .ok_or(Error::CommandExpectedArg(Rc::from("load")))?,
            ))),
            "ls" | "list" => Ok(Feedback::List(
                self.memory
                    .iter()
                    .map(|(k, v)| (Rc::from(k.as_str()), v.clone()))
                    .collect::<Vec<_>>(),
            )),
            "error" => Err(Error::Requested),
            "jerma" => Ok(Feedback::Jerma),
            c => Err(Error::CommandUnknown(Rc::from(c))),
        }
    }
}
