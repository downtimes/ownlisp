#[cfg(not(windows))]
extern crate editline;

extern crate pest;
#[macro_use]
extern crate pest_derive;

#[cfg(windows)]
use std::io::{self, Write};

use pest::Parser;

#[cfg(windows)]
mod editline {
  fn readline(param: String) -> Option<String> {
    print!(param);
    io::stdout().flush().unwrap_or_else(return None);
    let mut buffer = String::new();
    io::stdin()
      .read_line(&mut buffer)
      .unwrap_or_else(return None);
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
enum Operator {
  Plus,
  Minus,
  Divide,
  Multiply,
  Remainder,
}

impl Operator {
  fn parse(string: &str) -> Operator {
    match string {
      "-" => Operator::Minus,
      "+" => Operator::Plus,
      "*" => Operator::Multiply,
      "/" => Operator::Divide,
      "%" => Operator::Remainder,
      _ => panic!("Operator not implemented: {}", string),
    }
  }
}

#[derive(Debug)]
enum Expression {
  Number(i32),
  Hungarian(Operator, Vec<Expression>),
}

fn consume(pair: pest::iterators::Pair<Rule>) -> Expression {
  fn build_ast(pair: pest::iterators::Pair<Rule>) -> Expression {
    match pair.as_rule() {
      Rule::program => {
        let mut pairs = pair.into_inner();
        let decider = pairs.next().unwrap();
        if decider.as_rule() == Rule::number {
          build_ast(decider)
        } else {
          let op = Operator::parse(decider.as_str());
          let mut children = Vec::new();
          for pair in pairs {
            children.push(build_ast(pair));
          }
          Expression::Hungarian(op, children)
        }
      }

      Rule::number => Expression::Number(
        pair
          .as_str()
          .parse()
          .expect("We expect to only get valid numbers here"),
      ),

      Rule::expression => {
        let mut pairs = pair.into_inner();
        let decider = pairs
          .next()
          .expect("We expect expressions to have at least one child");
        if decider.as_rule() == Rule::number {
          build_ast(decider)
        } else {
          let op = Operator::parse(decider.as_str());
          let mut children = Vec::new();
          for pair in pairs {
            children.push(build_ast(pair));
          }
          Expression::Hungarian(op, children)
        }
      }

      _ => panic!(),
    }
  }
  build_ast(pair)
}

fn main() {
  println!("Ownlisp version 0.0.2");
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
      }
      None => break,
    }
  }
}
