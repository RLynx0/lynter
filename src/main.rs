use std::{
    fs,
    io::{self, stderr, stdin, stdout},
};

use crossterm::{
    cursor, execute,
    style::{Color, Print, Stylize},
    terminal::{self, ClearType},
    ExecutableCommand,
};
use interpreter::{Feedback, Interpreter};

mod interpreter;
mod lambda;

fn main() -> Result<(), io::Error> {
    println!("{}", include_str!("../static/greeting.txt"));

    let mut interpreter = Interpreter::new();
    let mut loadbuffer = Vec::new();

    loop {
        let input = match loadbuffer.pop() {
            Some(l) => String::from(l),
            None => {
                let mut input = String::new();
                stdout().execute(Print("λ> ".italic()))?;
                stdin().read_line(&mut input)?;
                input
            }
        };

        match interpreter.interpret(input.trim()) {
            Ok(Feedback::Bound { id, exp }) => {
                execute!(stdout(), Print(format!("{id} = {exp}\n").with(GREY)),)?
            }
            Ok(Feedback::Output { exp, matches }) => {
                execute!(stdout(), Print(" = "), Print(format!("{}\n", exp).blue()))?;
                if let Some(matches) = matches {
                    execute!(
                        stdout(),
                        Print(
                            format!(
                                "   {} == [{}]\n",
                                "^".repeat(exp.to_string().len()),
                                matches.join(", ")
                            )
                            .with(GREY)
                        )
                    )?
                }
            }
            Ok(Feedback::Load(path)) => {
                loadbuffer = match fs::read_to_string(path.as_ref()) {
                    Ok(c) => c.lines().map(|l| l.to_string()).rev().collect(),
                    Err(e) => {
                        execute!(stderr(), Print("ERROR".red().bold()), Print(e))?;
                        continue;
                    }
                }
            }
            Ok(Feedback::NoOp) => execute!(
                stdout(),
                cursor::MoveToPreviousLine(1),
                Print("   "),
                cursor::MoveToNextLine(1)
            )?,
            Ok(Feedback::Comment(c)) => execute!(
                stdout(),
                cursor::MoveToPreviousLine(1),
                terminal::Clear(ClearType::CurrentLine),
                Print(c.with(GREY)),
                cursor::MoveToNextLine(1)
            )?,
            Ok(Feedback::Quit) => break,
            Ok(Feedback::Help) => println!(include_str!("../static/help.txt")),
            Ok(Feedback::ClearScreen) => execute!(
                stdout(),
                terminal::Clear(ClearType::All),
                cursor::MoveTo(0, 0)
            )?,
            Ok(Feedback::List(mut list)) => {
                list.sort_by(|a, b| a.0.cmp(&b.0));
                for (id, exp) in list {
                    println!("{} = {}", id, exp)
                }
            }

            Ok(Feedback::Jerma) => println!(include_str!("../static/jerma.txt")),

            Err(e) => execute!(
                stderr(),
                Print("ERROR".red().bold()),
                Print(format!(": {}\n", e)),
            )?,
        }
    }

    println!("λynter exiting - Goodbye! :)");

    Ok(())
}

const GREY: Color = Color::Rgb {
    r: 128,
    g: 128,
    b: 128,
};
