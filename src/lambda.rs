use std::{convert::Infallible, fmt::Display};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exp<S> {
    Cal { func: Box<Exp<S>>, arg: Box<Exp<S>> },
    Fun { body: Box<Exp<S>> },
    Var { scope: usize },
    Sym(S),
}

impl<S> Exp<S> {
    fn has_symbols(&self) -> bool {
        match self {
            Exp::Cal { func, arg } => func.has_symbols() || arg.has_symbols(),
            Exp::Fun { body } => body.has_symbols(),
            Exp::Var { scope: _ } => false,
            Exp::Sym(_) => true,
        }
    }
}

impl<S> Exp<S>
where
    S: Clone + Eq,
{
    pub fn deep_reduce<R, E>(&self, resolve: R) -> Result<Self, E>
    where
        R: Fn(&S) -> Result<Self, E> + Copy,
    {
        let mut ret = self.reduce(resolve)?;

        loop {
            if !ret.has_symbols() {
                break Ok(ret);
            }
            let reduced = ret.reduce(resolve)?;
            if reduced == ret {
                break Ok(ret);
            }
            ret = reduced;
        }
    }
}

impl<S> Exp<S>
where
    S: Clone,
{
    pub fn reduce<R, E>(&self, resolve: R) -> Result<Self, E>
    where
        R: Fn(&S) -> Result<Self, E> + Copy,
    {
        match self {
            Exp::Cal { func, arg } => {
                if let Exp::Fun { body } = func.reduce(resolve)? {
                    body.insert(&arg.reduce(resolve)?, 1)
                        .update_outscoped_vars(|s| s - 1, 0)
                        .reduce(resolve)
                } else {
                    Ok(Exp::Cal {
                        func: Box::new(func.reduce(resolve)?),
                        arg: Box::new(arg.reduce(resolve)?),
                    })
                }
            }
            Exp::Fun { body } => Ok(Self::Fun {
                body: Box::new(body.reduce(resolve)?),
            }),
            Exp::Var { scope } => Ok(Exp::Var { scope: *scope }),
            // SAFETY: loop_mapper does not error
            Exp::Sym(s) => resolve(s),
        }
    }

    fn insert(&self, insert: &Self, depth: usize) -> Self {
        match self {
            Exp::Cal { func, arg } => Exp::Cal {
                func: Box::new(func.insert(insert, depth)),
                arg: Box::new(arg.insert(insert, depth)),
            },
            Exp::Fun { body } => Exp::Fun {
                body: Box::new(body.insert(insert, depth + 1)),
            },
            Exp::Var { scope } if *scope == depth => {
                insert.update_outscoped_vars(|s| s + *scope, 0)
            }
            _ => self.clone(),
        }
    }

    fn update_outscoped_vars<F>(&self, updater: F, depth: usize) -> Self
    where
        F: Fn(usize) -> usize + Copy,
    {
        match self {
            Exp::Cal { func, arg } => Exp::Cal {
                func: Box::new(func.update_outscoped_vars(updater, depth)),
                arg: Box::new(arg.update_outscoped_vars(updater, depth)),
            },
            Exp::Fun { body } => Exp::Fun {
                body: Box::new(body.update_outscoped_vars(updater, depth + 1)),
            },
            Exp::Var { scope } if *scope > depth => Exp::Var {
                scope: updater(*scope),
            },
            _ => self.clone(),
        }
    }
}

impl<S> Exp<S>
where
    S: Display,
{
    pub fn display(&self, depth: usize) -> String {
        match self {
            Exp::Cal { func, arg } => format!(
                "{} {}",
                match func.as_ref() {
                    Exp::Fun { body: _ } => format!("({})", func.display(depth)),
                    _ => func.display(depth),
                },
                match arg.as_ref() {
                    Exp::Cal { func: _, arg: _ } | Exp::Fun { body: _ } =>
                        format!("({})", arg.display(depth)),
                    _ => arg.display(depth),
                }
            ),
            Exp::Fun { body } => format!("{}.{}", varname(depth), body.display(depth + 1)),
            Exp::Var { scope } => match depth.checked_sub(*scope) {
                Some(order) => varname(order),
                None => String::from('?'),
            },
            Exp::Sym(s) => format!("[{s}]"),
        }
    }
}

impl<S> Display for Exp<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display(0))
    }
}

pub fn loop_mapper<S>(s: &S) -> Result<Exp<S>, Infallible>
where
    S: Clone,
{
    Ok(Exp::Sym(s.clone()))
}

const BASE_CHAR_U32: u32 = 'a' as u32;
const CHAR_RANGE: usize = 26;

fn varname(mut order: usize) -> String {
    let mut ret = String::new();
    loop {
        let char_u32 = BASE_CHAR_U32 + (order % CHAR_RANGE) as u32;
        // SAFETY: will always be a char between 'a' and 'z'
        ret.insert(0, char::from_u32(char_u32).unwrap());

        order /= 26;
        match order {
            0 => break ret,
            _ => order -= 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn varnames() {
        assert_eq!(varname(0), "a");
        assert_eq!(varname(1), "b");
        assert_eq!(varname(25), "z");
        assert_eq!(varname(26), "aa");
        assert_eq!(varname(54), "bc");

        for i in 0..1000 {
            println!("{}", varname(i));
        }
    }
}
