//TODO: Errors at the moment are pretty useless. We definitely need to improve
//our error reporting so people can actually fix their broken programs
#[cfg(not(windows))]
extern crate editline;

extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate failure;

use pest::Parser;
use std::collections::VecDeque;
use std::collections::HashMap;

const OP_MIN: &str = "min";
const OP_MAX: &str = "max";
const OP_PLUS: &str = "+";
const OP_MINUS: &str = "-";
const OP_EXP: &str = "^";
const OP_MULT: &str = "*";
const OP_DIV: &str = "/";
const OP_REM: &str = "rem";
const OP_LIST: &str = "list";
const OP_HEAD: &str = "head";
const OP_TAIL: &str = "tail";
const OP_JOIN: &str = "join";
const OP_EVAL: &str = "eval";

type Env<'a, 'b> = HashMap<&'a str, Ast<'b>>;
type LFunction<'a> = fn(Ast<'a>, Env) -> Ast<'a>;

mod editline {
  use std::io::{self, Write};

  pub(crate) fn readline(param: &str) -> Option<String> {
    print!("{}", param);
    if io::stdout().flush().is_err() {
      return None;
    }
    let mut buffer = String::new();
    if io::stdin().read_line(&mut buffer).is_err() {
      return None;
    }
    let old_len = buffer.len();
    //Strip the newline from the end
    buffer.truncate(old_len - 2);
    Some(buffer)
  }

  pub(crate) fn add_history(_history: &str) -> bool {
    true
  }
}

//This is done so we generate a new binary if we modify only the grammar
#[cfg(debug_assertions)]
const _GRAMMAR: &str = include_str!("ownlisp.pest");

#[derive(Parser)]
#[grammar = "ownlisp.pest"]
struct OwnlispParser;

#[derive(Debug)]
enum Ast<'a> {
  Number(i64),
  Symbol(&'a str),
  SExpression(VecDeque<Ast<'a>>),
  QExpression(VecDeque<Ast<'a>>),
  EmptyProgram,
}

impl<'a> Ast<'a> {
  fn is_qexpression(&self) -> bool {
    match self {
      Ast::QExpression(_) => true,
      _ => false,
    }
  }

  fn is_number(&self) -> bool {
    match self {
      Ast::Number(_) => true,
      _ => false,
    }
  }
}

impl<'a> std::fmt::Display for Ast<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Ast::Number(num) => write!(f, "{}", num),
      Ast::Symbol(sym) => write!(f, "{}", sym),
      Ast::SExpression(values) => {
        let mut res = write!(f, "(");
        let mut iter = values.into_iter().peekable();
        while let Some(item) = iter.next() {
          if iter.peek().is_some() {
            res = res.and(write!(f, "{} ", item));
          } else {
            res = res.and(write!(f, "{}", item));
          }
        }
        res = res.and(write!(f, ")"));
        res
      }
      Ast::QExpression(values) => {
        let mut res = write!(f, "{{");
        let mut iter = values.into_iter().peekable();
        while let Some(item) = iter.next() {
          if iter.peek().is_some() {
            res = res.and(write!(f, "{} ", item));
          } else {
            res = res.and(write!(f, "{}", item));
          }
        }
        res = res.and(write!(f, "}}"));
        res
      }
      Ast::EmptyProgram => write!(f, "()"),
    }
  }
}

fn apply_op<'a>(op: &str, x: Ast<'a>, y: Ast<'a>) -> Result<Ast<'a>, failure::Error> {
  match (x, y) {
    (Ast::Number(x), Ast::Number(y)) => {
      match op {
        OP_MINUS => Ok(Ast::Number(x - y)),
        OP_PLUS => Ok(Ast::Number(x + y)),
        OP_MULT => Ok(Ast::Number(x * y)),
        OP_DIV => {
          if y == 0 {
            bail!("Attempted division by 0.")
          } else {
            Ok(Ast::Number(x / y))
          }
        }
        OP_REM => Ok(Ast::Number(x % y)),
        //bODO: Check for if the conversation here is safe.
        OP_EXP => Ok(Ast::Number(x.pow(y as u32))),
        OP_MIN => Ok(Ast::Number(std::cmp::min(x, y))),
        OP_MAX => Ok(Ast::Number(std::cmp::max(x, y))),
        _ => panic!("We tried to apply a qexpression operator on a number"),
      }
    }
    _ => panic!("Apply op called with invalid arguments."),
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

      //TODO die ganze Sache mit operator usw muss hier weg und ein flexibles symbol
      //management muss her
      Rule::symbol => {
        Ast::Symbol(pair.as_str())
      }

      Rule::sexpression => {
        let res = pair.into_inner().map(build_ast).collect();
        Ast::SExpression(res)
      }

      Rule::qexpression => {
        let res = pair.into_inner().map(build_ast).collect();
        Ast::QExpression(res)
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
          Some(_) => Ast::SExpression(pairs.map(build_ast).collect()),
          None => Ast::EmptyProgram,
        }
      }

      _ => panic!("Unknown parsing rule encountered"),
    }
  }
  build_ast(pair)
}

fn evaluate_math_builtin<'a>(op: &str, sexp: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let sexp: Result<VecDeque<Ast>, failure::Error> = sexp.into_iter().map(|exp| evaluate(exp, env)).collect();
  let sexp = sexp?;
  //fast bailout if we don't only have numbers as operands
  if !sexp.iter().all(|ast| ast.is_number()) {
    println!("{:?}", sexp);
    bail!("Math functions only work with numbers.")
  }

  let mut values = sexp.into_iter().peekable();
  let first_operand = values.next();
  match first_operand {
    Some(operand) => {
      let mut total = operand;
      //we only had one operand so check if it's an unary operator
      if values.peek().is_none() {
        match op {
          OP_MINUS => {
            if let Ast::Number(x) = total {
              Ok(Ast::Number(-x))
            } else {
              unreachable!()
            }
          }
          OP_MIN | OP_MAX => Ok(total),
          _ => bail!("The operator {} is not an unary operator.", op),
        }
      //We have many operands
      } else {
        for val in values {
          total = apply_op(op, total, evaluate(val, env)?)?
        }
        Ok(total)
      }
    }
    None => bail!("Operators need an operand to work on."),
  }
}

fn evaluate_eval<'a>(expr: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let mut expr = expr;
  if expr.len() != 1 {
    bail!("Eval only works on a single Q-Expression as argument.")
  } else {
    let qexpr = evaluate(expr.pop_front().expect("We checked for size."), env)?;
    //unpack the QExpression
    if let Ast::QExpression(list) = qexpr {
      evaluate(Ast::SExpression(list), env)
    } else {
      bail!("Eval only works with QExpression as argument.")
    }
  }
}

fn evaluate_join<'a>(expr: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let expr: Result<VecDeque<Ast>, failure::Error> = expr.into_iter().map(|exp| evaluate(exp, env)).collect();
  let mut expr = expr?;
  if !expr.iter().all(|exp| exp.is_qexpression()) {
    bail!("Join can only work on Q-Expressions.")
  }

  match expr.pop_front() {
    Some(Ast::QExpression(mut res)) => {
      for exp in expr {
        if let Ast::QExpression(mut other) = exp {
          res.append(&mut other);
        } else {
          unreachable!()
        }
      }
      Ok(Ast::QExpression(res))
    }
    None => bail!("Join needs an argument to work on."),
    //We checked if it is some it has to be a QExpression
    _ => unreachable!(),
  }
}

fn evaluate_head<'a>(expr: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let mut expr = expr;
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr, env)?;
    if let Ast::QExpression(mut list) = qexpr {
      if let Some(first_elem) = list.pop_front() {
        let mut res = VecDeque::new();
        res.push_back(first_elem);
        Ok(Ast::QExpression(res))
      } else {
        bail!("Passed an empty Q-Expression to head")
      }
    } else {
      bail!("Head only works with a Q-Expression as argument.")
    }
  } else {
    bail!("Head works only with 1 argument.")
  }
}

fn evaluate_tail<'a>(expr: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let mut expr = expr;
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr, env)?;
    if let Ast::QExpression(mut list) = qexpr {
      if list.pop_front().is_some() {
        Ok(Ast::QExpression(list))
      } else {
        bail!("Passed an empty Q-Expression to tail")
      }
    } else {
      bail!("Tail only works with a Q-Expression as argument.")
    }
  } else {
    bail!("Tail works only with 1 argument.")
  }
}

fn evaluate_list(expr: VecDeque<Ast>) -> Ast {
  Ast::QExpression(expr)
}

fn evaluate_sexpression<'a>(sexp: VecDeque<Ast<'a>>, env: &mut Env) -> Result<Ast<'a>, failure::Error> {
  let mut sexp = sexp;
  match sexp.pop_front() {
    //we have an operand at the first position
    None => Ok(Ast::EmptyProgram),
    Some(Ast::Symbol(sym)) => match sym {
      OP_MAX
      | OP_MIN
      | OP_DIV
      | OP_MULT
      | OP_PLUS
      | OP_MINUS
      | OP_EXP
      | OP_REM => evaluate_math_builtin(sym, sexp, env),
      OP_JOIN => evaluate_join(sexp, env),
      OP_HEAD => evaluate_head(sexp, env),
      OP_TAIL => evaluate_tail(sexp, env),
      OP_EVAL => evaluate_eval(sexp, env),
      OP_LIST => Ok(evaluate_list(sexp)),
      _ => panic!("not yet implemented!")
    },

    //We have no Operator on the first position that's only okay if
    //We have no other values in the VecDeque!
    Some(val) => {
      if sexp.is_empty() {
        evaluate(val, env)
      } else {
        bail!("The first element of a S-Expression needs to be an operator.")
      }
    }
  }
}

fn evaluate<'a>(program: Ast<'a>, environment: &mut Env) -> Result<Ast<'a>, failure::Error> {
  match program {
    Ast::Function(_) => Ok(program),
    Ast::Number(_) => Ok(program),
    Ast::Symbol(_) => Ok(program),
    Ast::SExpression(values) => evaluate_sexpression(values, environment),
    //don't do anything for Q-Expressions
    Ast::QExpression(values) => Ok(Ast::QExpression(values)),
    _ => bail!("Unimplemented evaluation attempted."),
  }
}

fn fill_environment(env: &mut Env) {

}

fn main() {
  println!("Ownlisp version 0.0.3");
  println!("Press Ctrl+c to Exit\n");

  let mut environment = Env::new();
  fill_environment(&mut environment);

  while let Some(line) = editline::readline("Ownlisp>") {
    editline::add_history(&line);
    if line == "exit" {
      break;
    }
    let parsed = OwnlispParser::parse(Rule::program, line.trim());
    if let Ok(mut pairs) = parsed {
      let program = build_custom_ast(pairs.next().unwrap());
      //println!("{}", program);
      let evaluated = evaluate(program, &mut environment);
      match evaluated {
        Ok(result) => println!("{}", result),
        Err(err) => println!("{}", err),
      }
    } else {
      println!("Syntax error: {:?}", parsed.unwrap_err());
    }
  }
}
