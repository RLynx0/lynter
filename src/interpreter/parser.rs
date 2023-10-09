use std::{collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::lambda::Exp;

pub enum Parsed {
    Bind { id: String, exp: Exp<Rc<str>> },
    Pure(Exp<Rc<str>>),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected Token: '{0}'")]
    UnexpectedToken(Rc<str>),

    #[error("Expected closing Delimiter ')'")]
    ExpectedCloedParen,

    #[error("Found empty Expression")]
    EmptyExp,
}

#[derive(Clone, Copy)]
enum ParseExpect {
    ExpectRPar,
    NotExpectRPar,
}
use ParseExpect::*;

pub fn parse(input: &str) -> Result<Parsed, Error> {
    let tokens = tokenize(input);
    if let (Some(Token::Id(id)), Some(Token::Equals)) = (tokens.get(0), tokens.get(1)) {
        Ok(Parsed::Bind {
            id: id.to_string(),
            exp: parse_exp(&mut tokens[2..].iter(), HashMap::new(), 0, NotExpectRPar)?,
        })
    } else {
        Ok(Parsed::Pure(parse_exp(
            &mut tokens.iter(),
            HashMap::new(),
            0,
            NotExpectRPar,
        )?))
    }
}

fn parse_exp<'a>(
    tokens: &mut impl Iterator<Item = &'a Token>,
    mut id_stack: HashMap<String, usize>,
    depth: usize,
    expect_rparen: ParseExpect,
) -> Result<Exp<Rc<str>>, Error> {
    let mut ret: Option<Exp<Rc<str>>> = None;

    while let Some(token) = tokens.next() {
        let current_exp = match token {
            Token::Equals => return Err(Error::UnexpectedToken(Rc::from("="))),
            Token::LParen => parse_exp(tokens, id_stack.clone(), depth, ExpectRPar)?,
            Token::RParen => match expect_rparen {
                ExpectRPar => break,
                NotExpectRPar => return Err(Error::UnexpectedToken(Rc::from(")"))),
            },
            Token::Dot => match ret {
                Some(Exp::Sym(s)) => {
                    id_stack.insert(s.to_string(), depth);
                    ret = Some(Exp::Fun {
                        body: Box::new(parse_exp(
                            tokens,
                            id_stack.clone(),
                            depth + 1,
                            expect_rparen,
                        )?),
                    });
                    continue;
                }
                _ => return Err(Error::UnexpectedToken(Rc::from("."))),
            },
            Token::Id(i) => match id_stack.get(&i.to_string()) {
                Some(d) => Exp::Var { scope: depth - d },
                None => Exp::Sym(i.clone()),
            },
        };

        ret = match ret {
            Some(exp) => Some(Exp::Cal {
                func: Box::new(exp),
                arg: Box::new(current_exp),
            }),
            None => Some(current_exp),
        };
    }

    match ret {
        None => Err(Error::EmptyExp),
        Some(exp) => Ok(exp),
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    Equals,
    LParen,
    RParen,
    Dot,
    Id(Rc<str>),
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut ret = Vec::new();
    let mut word_start = 0;
    let mut cursor = 0;

    for char in input.chars() {
        if is_symbol(char) && word_start != cursor {
            ret.push(Token::Id(Rc::from(&input[word_start..cursor])));
        }

        cursor += char.len_utf8();

        match char {
            '=' => ret.push(Token::Equals),
            '(' => ret.push(Token::LParen),
            ')' => ret.push(Token::RParen),
            '.' => ret.push(Token::Dot),
            s if s.is_whitespace() => (),
            _ => continue,
        }

        word_start = cursor;
    }

    if word_start != cursor {
        ret.push(Token::Id(Rc::from(&input[word_start..cursor])));
    }

    ret
}

fn is_symbol(c: char) -> bool {
    match c {
        '=' | '(' | ')' | '.' => true,
        _ => c.is_whitespace(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_test() {
        use Token::*;
        assert_eq!(
            tokenize("I = a.a"),
            vec![
                Id(Rc::from("I")),
                Equals,
                Id(Rc::from("a")),
                Dot,
                Id(Rc::from("a"))
            ]
        );

        assert_eq!(
            tokenize("Kali!m = fergus. anar  . !!no% . = Po loc  moh"),
            vec![
                Id(Rc::from("Kali!m")),
                Equals,
                Id(Rc::from("fergus")),
                Dot,
                Id(Rc::from("anar")),
                Dot,
                Id(Rc::from("!!no%")),
                Dot,
                Equals,
                Id(Rc::from("Po")),
                Id(Rc::from("loc")),
                Id(Rc::from("moh")),
            ]
        );
    }
}
