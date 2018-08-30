use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use super::{ast, Ast, EnvId, Environments, OwnlispResult, Rule, OwnlispParser, evaluate};
use pest::Parser;

pub const LOAD: &str = "load";
pub const PRINT: &str = "print";
pub const ERROR: &str = "error";

//TODO: for the print and error call support some kind of variadic argument list with string interpolation

pub(crate) fn print(args: VecDeque<Ast>, _env: &mut Environments, _act_env: EnvId) -> OwnlispResult {
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

pub(crate) fn error(args: VecDeque<Ast>, _env: &mut Environments, _act_env: EnvId) -> OwnlispResult {
  let mut args = args;
  match args.pop_front() {
    Some(Ast::Str(s)) => {
      if !args.is_empty() {
        bail!("Too many arguments for the error call!")
      }
      bail!("Error: {}", s);
    }
    _ => bail!("Wrong or no argument type for the error call")
  }

}

pub(crate) fn load_ownlisp(args: VecDeque<Ast>, env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
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
      let parsed = OwnlispParser::parse(Rule::program, &contents);
      match parsed {
        Err(e) => bail!("Couldn't parse file: {}", e),
        Ok(mut pairs) => {
          let ast = ast::build(pairs.next().unwrap());
          //try to evaluate every expression seperately
          if let Ast::SExpression(sexp) = ast {
            for prog in sexp {
              match evaluate(prog, env, 0) {
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