use std::collections::VecDeque;
use std::rc::Rc;
use std::cell::RefCell;
use env::Env;
use super::{Ast, OwnlispResult, evaluate_sexpression};

pub const OP_GE: &str = ">=";
pub const OP_LE: &str = "<=";
pub const OP_LT: &str = "<";
pub const OP_GT: &str = ">";
pub const OP_EQ: &str = "=";
pub const OP_NE: &str = "!=";
pub const OP_IF: &str = "if";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";

pub(crate) fn evaluate_if(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  if args.len() != 3 {
    bail!("If needs three arguments!")
  }
  let mut args = args;
  if let Ast::Bool(b) = args.pop_front().expect("checked.") {
    if let Ast::QExpression(true_block) = args.pop_front().expect("checked.") {
      if let Ast::QExpression(false_block) = args.pop_front().expect("checked.") {
        if b {
          evaluate_sexpression(Ast::SExpression(true_block), env)
        } else {
          evaluate_sexpression(Ast::SExpression(false_block), env)
        }
      } else {
        bail!("Third argument to if needs to be a QExpression!")
      }
    } else {
      bail!("Second argument to if needs to ba a QExpression!")
    }
  } else {
    bail!("First argument to if needs to be a bool!")
  }
}

pub(crate) fn not_equal(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  let eq = equal(args, env);
  match eq {
    Ok(Ast::Bool(b)) => Ok(Ast::Bool(!b)),
    _ => eq,
  }
}

pub(crate) fn equal(args: VecDeque<Ast>, _env: &Rc<RefCell<Env>>) -> OwnlispResult {
  if args.len() != 2 {
    bail!("The equality operator only works with two arguments!")
  }
  let first = &args[0];
  let second = &args[1];
  Ok(Ast::Bool(first == second))
}

macro_rules! create_evalate_order {
  ($name:ident, $op:expr) => {
    pub(crate) fn $name(args: VecDeque<Ast>, _env: &Rc<RefCell<Env>>) -> OwnlispResult {
      evaluate($op, args)
    }
  };
}

create_evalate_order!(greater_then, OP_GT);
create_evalate_order!(lesser_then, OP_LT);
create_evalate_order!(greater_equal, OP_GE);
create_evalate_order!(lesser_equal, OP_LE);

fn evaluate(op: &str, args: VecDeque<Ast>) -> OwnlispResult {
  if args.len() != 2 {
    bail!("The comparison operators are only binary operators!")
  }
  let numbers = args
    .into_iter()
    .map(|arg| match arg {
      Ast::Number(num) => Ok(num),
      _ => Err(format_err!(
        "The comparison operators only work on numbers!"
      )),
    })
    .collect::<Result<Vec<_>, _>>()?;

  match op {
    OP_LT => Ok(Ast::Bool(numbers[0] < numbers[1])),
    OP_GT => Ok(Ast::Bool(numbers[0] > numbers[1])),
    OP_LE => Ok(Ast::Bool(numbers[0] <= numbers[1])),
    OP_GE => Ok(Ast::Bool(numbers[0] >= numbers[1])),
    _ => panic!("Unknown comparison operation!"),
  }
}