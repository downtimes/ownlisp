use super::{LFunction, VecDeque};
use std::{fmt};
use std::rc::Rc;
use std::cell::RefCell;
use env::Env;
use itertools::Itertools;

pub(crate) enum Ast {
  Number(i64),
  Bool(bool),
  Str(String),
  Builtin(LFunction),
  Function(Rc<RefCell<Env>>, VecDeque<String>, VecDeque<Ast>),
  Symbol(String),
  SExpression(VecDeque<Ast>),
  QExpression(VecDeque<Ast>),
  EmptyProgram,
}

impl Clone for Ast {
  fn clone(&self) -> Ast {
    match self {
      Ast::Number(num) => Ast::Number(*num),
      Ast::Bool(b) => Ast::Bool(*b),
      Ast::Str(s) => Ast::Str(s.clone()),
      Ast::Builtin(f) => Ast::Builtin(f.clone()),
      //Special case for cloning functions. We always open a new environment and
      //set it's parent to the same parent as the old one
      Ast::Function(env, form, body) => {
        let new_env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(env)))));
        let new_body = body.into_iter().cloned().collect();
        Ast::Function(new_env, form.clone(), new_body)
      }
      Ast::Symbol(s) => Ast::Symbol(s.clone()),
      Ast::SExpression(exp) => {
        let new_exp = exp.into_iter().cloned().collect();
        Ast::SExpression(new_exp)
      }
      Ast::QExpression(exp) => {
        let new_exp = exp.into_iter().cloned().collect();
        Ast::QExpression(new_exp)
      }
      Ast::EmptyProgram => Ast::EmptyProgram,
    }
  }
}

impl PartialEq for Ast {
  fn eq(&self, rhs: &Ast) -> bool {
    match (self, rhs) {
      (Ast::Number(a), Ast::Number(b)) => a == b,
      (Ast::Symbol(a), Ast::Symbol(b)) => a == b,
      (Ast::SExpression(a), Ast::SExpression(b)) => a == b,
      (Ast::QExpression(a), Ast::QExpression(b)) => a == b,
      (Ast::Str(a), Ast::Str(b)) => a == b,
      (Ast::Bool(a), Ast::Bool(b)) => a == b,
      (Ast::Builtin(a), Ast::Builtin(b)) => a == b,
      //NOTE: If we would check the environments too we would have an endless recursion
      (Ast::Function(_, formals1, body1), Ast::Function(_, formals2, body2)) => {
        formals1 == formals2 && body1 == body2
      }
      _ => false,
    }
  }
}

impl fmt::Display for Ast {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Ast::Number(num) => write!(f, "{}", num),
      Ast::Symbol(sym) => write!(f, "{}", sym),
      Ast::SExpression(values) => {
        write!(f, "({})", format!("{}", values.into_iter().format(" ")))
      }
      Ast::Str(s) => write!(f, "{}", s),
      Ast::Bool(b) => write!(f, "{}", b),
      Ast::Builtin(_) => write!(f, "<builtin>"),
      Ast::Function(_, formals, body) => {
        write!(f, "(\\ {{{}}} {{{}}}", 
               format!("{}", formals.into_iter().format(" ")),
               format!("{}", body.into_iter().format(" ")))
      }
      Ast::QExpression(values) => {
        write!(f, "{{{}}}", format!("{}", values.into_iter().format(" ")))
      }
      Ast::EmptyProgram => write!(f, "()"),
    }
  }
}
