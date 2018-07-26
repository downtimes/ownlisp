#[cfg(not(windows))]
extern crate editline;

extern crate pest;
#[macro_use]
extern crate pest_derive;

#[cfg(windows)]
use std::io::{self, Write};

use pest::Parser;
use std::io::{Write};

#[cfg(windows)]
mod editline {
    fn readline(param: String) -> Option<String> {
        print!(param);
        io::stdout().flush().unwrap_or_else(return None);
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap_or_else(return None);
        return Some(buffer);
    }

    fn add_history(history: &String) -> bool {
        true
    }
}

//This is done so we generate a new binary if we modify only the grammar
#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("ownlisp.pest");

#[derive(Parser)]
#[grammar = "ownlisp.pest"]
struct OwnlispParser;

#[derive(Debug)]
enum Operators {
    Plus,
    Minus,
    Times,
    Divide,
}

//Recursive Structure of our AST
#[derive(Debug)]
enum Program {
    Number(i32),
    Operator(Operators),
    Expression(Box<Program>),
}


fn consume(pair: pest::iterators::Pair<Rule>) -> Program {
    println!("{:?}", pair);
    fn value(pair: pest::iterators::Pair<Rule>) -> Program {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::program => { 
                Program::Number(0)
            }
            Rule::number => {
                Program::Number(pair.into_span().as_str().parse().unwrap())
            }
            Rule::expression => {
                if pair.into_inner().next().unwrap().as_rule() == Rule::number {
                    Program::Number(0)
                } else {
                    Program::Number(0)
                }
            }
            Rule::operator => {
                let op = pair.into_inner().next().unwrap().as_str();
                match op {
                    "-" => Program::Operator(Operators::Minus),
                    "+" => Program::Operator(Operators::Plus),
                    "/" => Program::Operator(Operators::Divide),
                    "*" => Program::Operator(Operators::Times),
                    _ => {
                        unreachable!();
                    }
                }
            }
            _ => { unreachable!(); }
        }
    }

    value(pair)
}

fn main() {
    println!("Ownlisp version 0.0.1");
    println!("Press Ctrl+c to Exit\n");


    loop {
        match editline::readline("Ownlisp>") {
            Some(line) => { 
                editline::add_history(line);
                let parsed = OwnlispParser::parse(Rule::program, line.trim());
                if let Ok(mut pairs) = parsed {
                    let ast = consume(pairs.next().unwrap());
                    println!("{:?}", ast);
                } else {
                    println!("You fucked up boy: {:?}", parsed.unwrap_err());
                }
            },
            None => break
        }
    }
}
