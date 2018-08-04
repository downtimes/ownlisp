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


#[derive(Debug, Fail)]
enum EvalError {
  #[fail(display = "Attempted division by 0")]
  DivisonByZero,
  #[fail(display = "Expected an Operand")]
  MissingOperand,
  #[fail(display = "Expected an Operator")]
  MissingOperator,
  #[fail(display = "Operator in the middle of nowhere")]
  OperatorPlacement,
  #[fail(display = "Not a unary Operator")]
  NotUnary,
  #[fail(display = "Wrong usage of unary Oparator. They only work on numbers")]
  WrongUnary
}


#[derive(Debug)]
enum LVal {
  Number(i64),
  Error(EvalError),
  EmptyProgram,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl Operator {
  fn parse(string: &str) -> Operator {
    match string {
      "-" => Operator::Minus,
      "+" => Operator::Plus,
      "*" => Operator::Multiply,
      "/" => Operator::Divide,
      "%" => Operator::Remainder,
      "^" => Operator::Exp,
      "min" => Operator::Min,
      "max" => Operator::Max,
      _ => panic!("Unknown operator"),
    }
  }
}

fn apply_op(op: Operator, x: LVal, y: LVal) -> LVal {
  match (x, y) {
    (LVal::Number(x), LVal::Number(y)) => {
      match op {
        Operator::Minus => LVal::Number(x - y),
        Operator::Plus => LVal::Number(x + y),
        Operator::Multiply => LVal::Number(x * y),
        Operator::Divide => {
          if y == 0 {
            LVal::Error(EvalError::DivisonByZero)
          } else {
            LVal::Number(x / y)
          }
        }
        Operator::Remainder => LVal::Number(x % y),
        //TODO: Check for if the conversation here is safe.
        Operator::Exp => LVal::Number(x.pow(y as u32)),
        Operator::Min => LVal::Number(std::cmp::min(x, y)),
        Operator::Max => LVal::Number(std::cmp::max(x, y)),
      }
    }
    //If we already had an Error value in one of our operands bubble it up
    (LVal::Error(x), _) => LVal::Error(x),
    (_, LVal::Error(y)) => LVal::Error(y),
    (_, _) => LVal::EmptyProgram,
  }
}

#[derive(Debug, Clone)]
enum SExpression {
  Number(i32),
  Operator(Operator),
  Children(Vec<SExpression>),
}

fn consume(pair: pest::iterators::Pair<Rule>) -> SExpression {
  fn build_ast(pair: pest::iterators::Pair<Rule>) -> SExpression {
    match pair.as_rule() {
      Rule::number => SExpression::Number(
        pair
          .as_str()
          .parse()
          .expect("We expect to only get valid numbers here"),
      ),

      Rule::operator => SExpression::Operator(
        Operator::parse(pair.as_str())
      ),

      Rule::sexpression => {
        let res = pair.into_inner().map(|pair| build_ast(pair)).collect();
        SExpression::Children(res)
      }

      Rule::expression => {
        build_ast(pair.into_inner().next().expect("Expressions always consist of one item"))
      }

      Rule::program => { 
        SExpression::Children(pair.into_inner().map(|pair| build_ast(pair)).collect())
      }

      _ => panic!("Unknown parsing rule encountered"),
    }
  }
  build_ast(pair)
}

fn evaluate(program: SExpression) -> LVal {
  match program {
    SExpression::Number(x) => LVal::Number(x as i64),
    SExpression::Operator(_) => LVal::Error(EvalError::OperatorPlacement),
    SExpression::Children(values) => {
      if let Some(first) = values.iter().next() {
        match first {
          //we have an operand at the first position
          SExpression::Operator(ref op) => {
            let operands = values.iter().skip(1).collect::<Vec<_>>();
            if operands.len() == 1 {
              if let SExpression::Number(x) = operands[0] {
                if *op == Operator::Minus {
                  LVal::Number(-(*x) as i64)
                } else if *op == Operator::Max || *op == Operator::Min {
                  LVal::Number(*x as i64)
                } else {
                  LVal::Error(EvalError::NotUnary)
                }
              } else {
                LVal::Error(EvalError::WrongUnary)
              }
            } else {
              let mut total = evaluate(operands[0].clone());
              for &val in operands.iter().skip(1) {
                total = apply_op(*op, total, evaluate(val.clone()));
              }
              total
            }
          }
          //We don't have an operand at the firs positio
          _ => {
            //That's onyl okay if it is a single number or a single vec
            //and we can evaluate theese like normal
            if values.len() == 1 {
              evaluate(first.clone())
            } else {
              LVal::Error(EvalError::MissingOperator)
            }
          }
        }
      } else {
        LVal::EmptyProgram
      }
    },
  }
}

fn main() {
  println!("Ownlisp version 0.0.3");
  println!("Press Ctrl+c to Exit\n");

  loop {
    match editline::readline("Ownlisp>") {
      Some(line) => {
        editline::add_history(line);
        let parsed = OwnlispParser::parse(Rule::program, line.trim());
        if let Ok(mut pairs) = parsed {
          let program = consume(pairs.next().unwrap());
          println!("{:?}", program);
          let evaluated = evaluate(program);
          match evaluated {
            LVal::Number(num) => println!("{}", num),
            LVal::Error(err) => println!("{}", err),
            LVal::EmptyProgram => println!("{}", "()")
          }
        } else {
          println!("Syntax error: {:?}", parsed.unwrap_err());
        }
      }
      None => break,
    }
  }
}
