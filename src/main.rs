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
const OP_LIST: &str = "list";
const OP_HEAD: &str = "head";
const OP_TAIL: &str = "tail";
const OP_JOIN: &str = "join";
const OP_EVAL: &str = "eval";
const OP_LEN: &str = "len";

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

//TODO: instead of carrying around an error value in the AST do error handleing
//by converting the functions into Result<Ast, failure::Error> to make it more
//idiomatic and robust
#[derive(Debug)]
enum Ast {
  Number(i64),
  Operator(Operator),
  SExpression(VecDeque<Ast>),
  QExpression(VecDeque<Ast>),
  Error(failure::Error),
  EmptyProgram,
}

impl Ast {
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

impl std::fmt::Display for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Ast::Number(num) => write!(f, "{}", num),
      Ast::Operator(op) => write!(f, "{}", op),
      Ast::Error(err) => write!(f, "{}", err),
      Ast::SExpression(values) => {
        let mut res = write!(f, "(");
        //We need special casing for the last element to not print it with the
        //space after. So we need to check we actually have elements and our
        //calculation of the last element index is valid.
        if values.len() > 0 {
          for idx in 0..values.len() - 1 {
            res = res.and(write!(f, "{} ", values[idx]));
          }
          res = res.and(write!(f, "{}", values[values.len() - 1]));
        }
        res = res.and(write!(f, ")"));
        res
      }
      Ast::QExpression(values) => {
        let mut res = write!(f, "{}", "{");
        //Same as with the branch above for SExpression. Special casing for last
        //element makes code a little ugly.
        if values.len() > 0 {
          for idx in 0..values.len() - 1 {
            res = res.and(write!(f, "{} ", values[idx]));
          }
          res = res.and(write!(f, "{}", values[values.len() - 1]));
        }
        res = res.and(write!(f, "{}", "}"));
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
  List,
  Eval,
  Head,
  Tail,
  Join,
  Len,
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
      Operator::Eval => write!(f, "{}", OP_EVAL),
      Operator::Head => write!(f, "{}", OP_HEAD),
      Operator::Tail => write!(f, "{}", OP_TAIL),
      Operator::Join => write!(f, "{}", OP_JOIN),
      Operator::List => write!(f, "{}", OP_LIST),
      Operator::Len => write!(f, "{}", OP_LEN),
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
      OP_EVAL => Ok(Operator::Eval),
      OP_LIST => Ok(Operator::List),
      OP_HEAD => Ok(Operator::Head),
      OP_TAIL => Ok(Operator::Tail),
      OP_JOIN => Ok(Operator::Join),
      OP_LEN => Ok(Operator::Len),
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
        _ => panic!("We tried to apply a qexpression operator on a number"),
      }
    }
    //If we already had an Error value in one of our operands bubble it up.
    (Ast::Error(x), _) | (_, Ast::Error(x)) => Ast::Error(x),
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

      Rule::operator => Ast::Operator(pair.as_str().parse().expect("Unknown operator encountered")),

      Rule::sexpression => {
        let res = pair.into_inner().map(|pair| build_ast(pair)).collect();
        Ast::SExpression(res)
      }

      Rule::qexpression => {
        let res = pair.into_inner().map(|pair| build_ast(pair)).collect();
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
          Some(_) => Ast::SExpression(pairs.map(|pair| build_ast(pair)).collect()),
          None => Ast::EmptyProgram,
        }
      }

      _ => panic!("Unknown parsing rule encountered"),
    }
  }
  build_ast(pair)
}

fn evaluate_math_builtin(op: Operator, sexp: VecDeque<Ast>) -> Ast {
  //fast bailout if we don't only have numbers as operands
  if !sexp.iter().all(|ast| ast.is_number()) {
    return Ast::Error(format_err!("Math functions only work with numbers."));
  }

  let mut values = sexp.into_iter().peekable();
  let first_operand = values.next();
  match first_operand {
    Some(operand) => {
      let mut total = operand;
      //we only had one operand so check if it's an unary operator
      if let None = values.peek() {
        match op {
          Operator::Minus => {
            if let Ast::Number(x) = total {
              Ast::Number(-x)
            } else {
              unreachable!()
            }
          }
          Operator::Max | Operator::Min => total,
          _ => Ast::Error(format_err!("The operator {} is not an unary operator.", op)),
        }
      //We have many operands
      } else {
        for val in values {
          total = apply_op(op.clone(), total, evaluate(val));
        }
        total
      }
    }
    None => Ast::Error(format_err!("Operators need an operand to work on.")),
  }
}

fn evaluate_eval(expr: VecDeque<Ast>) -> Ast {
  let mut expr = expr;
  if expr.len() != 1 {
    Ast::Error(format_err!(
      "Eval only works on a single Q-Expression as argument."
    ))
  } else {
    let qexpr = evaluate(expr.pop_front().expect("We checked for size."));
    //unpack the QExpression 
    if let Ast::QExpression(list) = qexpr {
      evaluate(Ast::SExpression(list))
    } else {
      Ast::Error(format_err!("Eval only works with QExpression as argument."))
    }
  }
}

fn evaluate_join(expr: VecDeque<Ast>) -> Ast {
  let mut expr: VecDeque<Ast> = expr.into_iter().map(|exp| evaluate(exp)).collect();
  if !expr.iter().all(|exp| exp.is_qexpression()) {
    return Ast::Error(format_err!("Join can only work on Q-Expressions."));
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
      Ast::QExpression(res)
    }
    None => Ast::Error(format_err!("Join needs an argument to work on.")),
    //We checked if it is some it has to be a QExpression
    _ => unreachable!(),
  }
}

fn evaluate_head(expr: VecDeque<Ast>) -> Ast {
  let mut expr = expr;
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr);
    if let Ast::QExpression(mut list) = qexpr {
      if let Some(first_elem) = list.pop_front() {
        let mut res = VecDeque::new();
        res.push_back(first_elem);
        Ast::QExpression(res)
      } else {
        Ast::Error(format_err!("Passed an empty Q-Expression to head"))
      }
    } else {
      Ast::Error(format_err!(
        "Head only works with a Q-Expression as argument."
      ))
    }
  } else {
    Ast::Error(format_err!("Head works only with 1 argument."))
  }
}

fn evaluate_tail(expr: VecDeque<Ast>) -> Ast {
  let mut expr = expr;
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr);
    if let Ast::QExpression(mut list) = qexpr {
      if let Some(_) = list.pop_front() {
        Ast::QExpression(list)
      } else {
        Ast::Error(format_err!("Passed an empty Q-Expression to tail"))
      }
    } else {
      Ast::Error(format_err!(
        "Tail only works with a Q-Expression as argument."
      ))
    }
  } else {
    Ast::Error(format_err!("Tail works only with 1 argument."))
  }
}

fn evaluate_list(expr: VecDeque<Ast>) -> Ast {
  Ast::QExpression(expr)
}

fn evaluate_len(expr: VecDeque<Ast>) -> Ast {
  let mut expr = expr;
  if expr.len() != 1 {
    Ast::Error(format_err!("Len only works with 1 argument."))
  } else {
    let first_expr = expr.pop_front().expect("we checked for size.");
    let qexpr = evaluate(first_expr);
    if let Ast::QExpression(list) = qexpr {
      Ast::Number(list.len() as i64)
    } else {
      Ast::Error(format_err!("Len only works on a Q-Expression."))
    }
  }

}

fn evaluate_sexpression(sexp: VecDeque<Ast>) -> Ast {
  let mut sexp = sexp;
  match sexp.pop_front() {
    //we have an operand at the first position
    None => Ast::EmptyProgram,
    Some(Ast::Operator(op)) => match op {
      Operator::Max
      | Operator::Min
      | Operator::Divide
      | Operator::Multiply
      | Operator::Exp
      | Operator::Minus
      | Operator::Plus
      | Operator::Remainder => evaluate_math_builtin(op, sexp),
      Operator::Join => evaluate_join(sexp),
      Operator::Head => evaluate_head(sexp),
      Operator::Tail => evaluate_tail(sexp),
      Operator::Eval => evaluate_eval(sexp),
      Operator::List => evaluate_list(sexp),
      Operator::Len => evaluate_len(sexp),
    },

    //We have no Operator on the first position that's only okay if
    //We have no other values in the VecDeque!
    Some(val) => {
      if sexp.len() == 0 {
        evaluate(val)
      } else {
        Ast::Error(format_err!(
          "The first element of a S-Expression needs to be an operator."
        ))
      }
    }
  }
}

fn evaluate(program: Ast) -> Ast {
  match program {
    Ast::Number(_) => program,
    Ast::Operator(_) => Ast::Error(format_err!(
      "Operator placed in invalid position. Operators are only allowed at the first position."
    )),
    Ast::SExpression(values) => evaluate_sexpression(values),
    //don't do anything for Q-Expressions
    Ast::QExpression(values) => Ast::QExpression(values),
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
          //println!("{}", program);
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
