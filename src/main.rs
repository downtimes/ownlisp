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
  #[fail(display = "Attempted division by 0.")]
  DivisonByZero,
  #[fail(display = "Expected an Operator.")]
  MissingOperator,
  #[fail(display = "Operator in the middle of nowhere.")]
  OperatorPlacement,
  #[fail(display = "Not a unary Operator.")]
  NotUnary,
  #[fail(display = "Wrong usage of unary Oparator. They only work on numbers.")]
  WrongUnary,
  #[fail(display = "Operators need an Operand to work on.")]
  NoOperand
}


#[derive(Debug)]
enum LVal {
  Number(i64),
  Error(EvalError),
  EmptyProgram,
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

#[derive(Debug)]
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

//TODO: the cloning of SExpressions here is extremely bad for performance
//look into how we can handle it without needing to clone stuff
//somehow we need to spli the first element out of the iterator and get a 
//iterator of the remaining rest
fn evaluate(program: SExpression) -> LVal {
  match program {
    SExpression::Number(x) => LVal::Number(x as i64),
    SExpression::Operator(_) => LVal::Error(EvalError::OperatorPlacement),
    SExpression::Children(values) => {
      let mut values = values.into_iter().peekable();
      if let Some(first) = values.next() {
        match first {
          //we have an operand at the first position
          SExpression::Operator(op) => {
            let first_operand = values.next();
            match first_operand {
              Some(operand) => {
                let mut total = evaluate(operand);
                //We only have one operand so check if it's an operator that 
                //works unary
                if let None = values.peek() {
                  //Check that we have a number as operand
                  if let LVal::Number(num) = total {
                    match op {
                      Operator::Minus => {
                        LVal::Number(-num)
                      }

                      Operator::Max | Operator::Min => {
                        LVal::Number(num)
                      }

                      _ => LVal::Error(EvalError::NotUnary)
                    }
                  } else {
                    LVal::Error(EvalError::WrongUnary)
                  }
                } else {
                  for val in values {
                    total = apply_op(op.clone(), total, evaluate(val));
                  }
                  total
                }
              }
              None => LVal::Error(EvalError::NoOperand)
            }
          }

          //We have no Operator on the first position that's only okay if
          //We have no other values in the vector!
          _ => {
            if values.len() == 0 {
              evaluate(first)
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
        if line == "exit" {
          break
        }
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
