#[cfg(not(windows))]
extern crate editline;

extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate failure;

#[cfg(windows)]
use std::io::{self, Write};

use pest::Parser;
use std::collections::VecDeque;

const OP_MIN: &str = "min";
const OP_MAX: &str = "max";
const OP_PLUS: &str = "+";
const OP_MINUS: &str = "-";
const OP_EXP: &str = "^";
const OP_MULT: &str = "*";
const OP_DIV: &str = "/";
const OP_REM: &str = "rem";


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
enum Ast {
  Program(Vec<Ast>),
  Number(i64),
  Operator(Operator),
  SExpression(VecDeque<Ast>),
  QExpression(VecDeque<Ast>),
  Error(failure::Error),
  EmptyProgram,
}

impl std::fmt::Display for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Ast::Program(prog) => {
        let mut res = write!(f, "(");
        for idx in 0..prog.len() - 1 {
          res = res.and(write!(f, "{} ", prog[idx]));
        }
        res = res.and(write!(f, "{})", prog[prog.len() - 1]));
        res
      }
      Ast::Number(num) => write!(f, "{}", num),
      Ast::Operator(op) => write!(f, "{}", op),
      Ast::Error(err) => write!(f, "{}", err),
      Ast::SExpression(values) | Ast::QExpression(values) => {
        let mut res = write!(f, "(");
        if values.len() > 0 {
          for idx in 0..values.len() - 1 {
            res = res.and(write!(f, "{} ", values[idx]));
          }
          res = res.and(write!(f, "{}", values[values.len() - 1]));
        }
        res = res.and(write!(f, ")"));
        res
      }
      Ast::EmptyProgram => write!(f, "()"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operator {
  Plus,
  Minus,
  Divide,
  Multiply,
  Remainder,
  Exp,
  Min,
  Max,
}

impl std::fmt::Display for Operator {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {  
    match self {
      Operator::Plus => write!(f, "{}", OP_PLUS),
      Operator::Minus => write!(f, "{}", OP_MINUS),
      Operator::Divide => write!(f, "{}", OP_DIV),
      Operator::Multiply => write!(f, "{}", OP_MULT),
      Operator::Remainder => write!(f, "{}", OP_REM),
      Operator::Exp => write!(f, "{}", OP_EXP),
      Operator::Min => write!(f, "{}", OP_MIN),
      Operator::Max => write!(f, "{}", OP_MAX),
    }
  }
}

#[derive(Debug)]
struct ParseOperatorError;

impl std::str::FromStr for Operator { 
  type Err = ParseOperatorError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      OP_MINUS => Ok(Operator::Minus),
      OP_PLUS => Ok(Operator::Plus),
      OP_MULT => Ok(Operator::Multiply),
      OP_DIV => Ok(Operator::Divide),
      OP_REM => Ok(Operator::Remainder),
      OP_EXP => Ok(Operator::Exp),
      OP_MIN => Ok(Operator::Min),
      OP_MAX => Ok(Operator::Max),
      _ => Err(ParseOperatorError),
    }
  }
}

fn apply_op(op: Operator, x: Ast, y: Ast) -> Ast {
  match (x, y) {
    (Ast::Number(x), Ast::Number(y)) => {
      match op {
        Operator::Minus => Ast::Number(x - y),
        Operator::Plus => Ast::Number(x + y),
        Operator::Multiply => Ast::Number(x * y),
        Operator::Divide => {
          if y == 0 {
            Ast::Error(format_err!("Attempted division by 0."))
          } else {
            Ast::Number(x / y)
          }
        }
        Operator::Remainder => Ast::Number(x % y),
        //TODO: Check for if the conversation here is safe.
        Operator::Exp => Ast::Number(x.pow(y as u32)),
        Operator::Min => Ast::Number(std::cmp::min(x, y)),
        Operator::Max => Ast::Number(std::cmp::max(x, y)),
      }
    }
    //If we already had an Error value in one of our jperands bubble it up
    (Ast::Error(x), _) | (_, Ast::Error(x)) => Ast::Error(x),
    (Ast::EmptyProgram, _) | (_, Ast::EmptyProgram) => Ast::EmptyProgram,
    _ => panic!(
      "Apply op called with SExpression or QEXpression."
    ),
  }
}

fn build_custom_ast(pair: pest::iterators::Pair<Rule>) -> Ast {
  fn build_ast(pair: pest::iterators::Pair<Rule>) -> Ast {
    match pair.as_rule() {
      Rule::number => Ast::Number(
        pair
          .as_str()
          .parse()
          .expect("We expect to only get valid numbers here"),
      ),

      Rule::operator => Ast::Operator(pair.as_str().parse().expect("Unknown operator encountered")),

      Rule::sexpression => {
        let res = pair.into_inner().map(|pair| build_ast(pair)).collect();
        Ast::SExpression(res)
      }

      Rule::expression => build_ast(
        pair
          .into_inner()
          .next()
          .expect("Expressions always consist of one item"),
      ),

      Rule::program => {
        let mut pairs = pair.into_inner().peekable();
        match pairs.peek() {
          Some(_) => Ast::Program(pairs.map(|pair| build_ast(pair)).collect()),
          None => Ast::EmptyProgram,
        }
      }

      _ => panic!("Unknown parsing rule encountered"),
    }
  }
  build_ast(pair)
}

fn evaluate_sexpression(sexp: VecDeque<Ast>) -> Ast {
  let mut values = sexp.into_iter().peekable();
  if let Some(first) = values.next() {
    match first {
      //we have an operand at the first position
      Ast::Operator(op) => {
        let first_operand = values.next();
        match first_operand {
          Some(operand) => {
            let mut total = evaluate(operand);
            //We only have one operand so check if it's an operator that
            //works unary
            if let None = values.peek() {
              //Check that we have a number as operand
              if let Ast::Number(num) = total {
                match op {
                  Operator::Minus => Ast::Number(-num),

                  Operator::Max | Operator::Min => Ast::Number(num),

                  _ => Ast::Error(format_err!(
                    "The operator {} is not an unary Operator.",
                    op
                  )),
                }
              } else {
                Ast::Error(format_err!("Unary oprands can only operate on numbers."))
              }
            } else {
              for val in values {
                total = apply_op(op.clone(), total, evaluate(val));
              }
              total
            }
          }
          None => Ast::Error(format_err!(
            "The first element of a S-Expression needs to be an operator.",
          )),
        }
      }

      //We have no Operator on the first position that's only okay if
      //We have no other values in the vector!
      _ => {
        if values.len() == 0 {
          evaluate(first)
        } else {
          Ast::Error(format_err!(
            "The first element of a S-Expression needs to be an operator."
          ))
        }
      }
    }
  } else {
    Ast::EmptyProgram
  }
}

fn evaluate(program: Ast) -> Ast {
  match program {
    Ast::Program(expressions) => {
      //TODO: think of something more elegant here
      evaluate(Ast::SExpression(expressions.into_iter().collect()))
    }
    Ast::Number(_) => program,
    Ast::Operator(_) => Ast::Error(format_err!(
      "Operator placed in invalid position. Operators are only allowed at the first position."
    )),
    Ast::SExpression(values) => evaluate_sexpression(values),
    _ => Ast::Error(format_err!("Unimplemented evaluation attempted.")),
  }
}

fn main() {
  println!("Ownlisp version 0.0.3");
  println!("Press Ctrl+c to Exit\n");

  loop {
    match editline::readline("Ownlisp>") {
      Some(line) => {
        editline::add_history(line);
        if line == "exit" {
          break;
        }
        let parsed = OwnlispParser::parse(Rule::program, line.trim());
        if let Ok(mut pairs) = parsed {
          let program = build_custom_ast(pairs.next().unwrap());
          println!("{}", program);
          let evaluated = evaluate(program);
          println!("{}", evaluated);
        } else {
          println!("Syntax error: {:?}", parsed.unwrap_err());
        }
      }
      None => break,
    }
  }
}
