use super::{LFunction, VecDeque};
use std::{cmp, ptr, fmt};
use std::rc::Rc;
use std::cell::RefCell;
use env::{Env};

pub(crate) enum Ast {
  Number(i64),
  Bool(bool),
  Str(String),
  Builtin(LFunction),
  //TODO: implement drop on ast so that Function can cleanup their EnvId's in the map!
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
        let new_body = body.into_iter().map(|ast| ast.clone()).collect();
        Ast::Function(new_env, form.clone(), new_body)
      }
      Ast::Symbol(s) => Ast::Symbol(s.clone()),
      Ast::SExpression(exp) => {
        let new_exp = exp.into_iter().map(|ast| ast.clone()).collect();
        Ast::SExpression(new_exp)
      }
      Ast::QExpression(exp) => {
        let new_exp = exp.into_iter().map(|ast| ast.clone()).collect();
        Ast::QExpression(new_exp)
      }
      Ast::EmptyProgram => Ast::EmptyProgram,
    }
  }
}

//TODO: cleanup!
impl cmp::PartialEq for Ast {
  fn eq(&self, other: &Ast) -> bool {
    if let Ast::Number(mine) = self {
      if let Ast::Number(other) = other {
        mine == other
      } else {
        false
      }
    } else if let Ast::Bool(mine) = self {
      if let Ast::Bool(other) = other {
        mine == other
      } else {
        false
      }
    } else if let Ast::Builtin(mine) = self {
      if let Ast::Builtin(other) = other {
        ptr::eq(&mine, &other)
      } else {
        false
      }
    } else if let Ast::Function(_, mine2, mine3) = self {
      if let Ast::Function(_, other2, other3) = other {
        mine2 == other2 && mine3 == other3
      } else {
        false
      }
    } else if let Ast::Symbol(mine) = self {
      if let Ast::Symbol(other) = other {
        mine == other
      } else {
        false
      }
    } else if let Ast::SExpression(mine) = self {
      if let Ast::SExpression(other) = other {
        if mine.len() != other.len() {
          false
        } else {
          for (m, o) in mine.iter().zip(other.iter()) {
            if m != o {
              return false;
            }
          }
          true
        }
      } else {
        false
      }
    } else if let Ast::Str(mine) = self {
      if let Ast::Str(other) = other {
        mine == other
      } else {
        false
      }
    } else if let Ast::QExpression(mine) = self {
      if let Ast::QExpression(other) = other {
        if mine.len() != other.len() {
          false
        } else {
          for (m, o) in mine.iter().zip(other.iter()) {
            if m != o {
              return false;
            }
          }
          true
        }
      } else {
        false
      }
    } else {
      true
    }
  }
}

impl fmt::Display for Ast {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
      Ast::Str(s) => write!(f, "{}", s),
      Ast::Bool(b) => write!(f, "{}", b),
      Ast::Builtin(_) => write!(f, "<builtin>"),
      Ast::Function(_, formals, body) => {
        let mut res = write!(f, "(\\ {{");
        let mut formals = formals.into_iter().peekable();
        while let Some(formal) = formals.next() {
          if formals.peek().is_some() {
            res = res.and(write!(f, "{} ", formal));
          } else {
            res = res.and(write!(f, "{}", formal));
          }
        }
        res = res.and(write!(f, "}} {{"));
        let mut body = body.into_iter().peekable();
        while let Some(bod) = body.next() {
          if body.peek().is_some() {
            res = res.and(write!(f, "{} ", bod));
          } else {
            res = res.and(write!(f, "{}", bod));
          }
        }
        res = res.and(write!(f, "}})"));
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
