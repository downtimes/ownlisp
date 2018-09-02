use super::{evaluate, Ast, OwnlispResult};
use parser;
use env::Env;
use std::collections::VecDeque;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::File;
use std::io::prelude::*;

pub const LOAD: &str = "load";
pub const PRINT: &str = "print";
pub const ERROR: &str = "error";

//TODO: for the print and error call support some kind of variadic argument list with string interpolation
//TODO: for print support some kind of string interpolation
//TODO: all these functions don't care about the environment stuff. That smells.

pub(crate) fn print(
  args: VecDeque<Ast>,
  _env: &Rc<RefCell<Env>>,
) -> OwnlispResult {
  let mut args = args;
  match args.pop_front() {
    Some(Ast::Str(s)) => {
      if !args.is_empty() {
        bail!("Too many arguments for the print call!")
      }
      println!("{}", s);
      Ok(Ast::EmptyProgram)
    }
    _ => bail!("Wrong argument type for the print call!"),
  }
}

pub(crate) fn error(
  args: VecDeque<Ast>,
  _env: &Rc<RefCell<Env>>,
) -> OwnlispResult {
  let mut args = args;
  match args.pop_front() {
    Some(Ast::Str(s)) => {
      if !args.is_empty() {
        bail!("Too many arguments for the error call!")
      }
      bail!("Error: {}", s);
    }
    _ => bail!("Wrong or no argument type for the error call"),
  }
}

pub(crate) fn load_ownlisp(
  args: VecDeque<Ast>,
  env: &Rc<RefCell<Env>>,
) -> OwnlispResult {
  let mut args = args;
  match args.pop_front() {
    Some(Ast::Str(s)) => {
      if !args.is_empty() {
        bail!("load only works with one string argument.")
      }
      let file_name = s.trim_matches('"');
      let mut f = match File::open(file_name) {
        Ok(f) => f,
        Err(e) => bail!("Could not open file {}! ({})", s, e),
      };
      let mut contents = String::new();
      f.read_to_string(&mut contents)?;
      match parser::parse_to_ast(&contents) {
        Err(e) => bail!("Couldn't parse file: {}", e),
        Ok(mut ast) => {
          if let Ast::SExpression(sexp) = ast {
            for expr in sexp.into_iter() {
              match evaluate(expr, env) {
                Ok(_) => continue,
                Err(e) => return Err(e),
              }
            }
          }
          Ok(Ast::EmptyProgram)
        }
      }
    }
    _ => bail!("load only works with strings as argument."),
  }
}
