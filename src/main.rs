//TODO: Errors at the moment are pretty useless. We definitely need to improve
//our error reporting so people can actually fix their broken programs
//TODO: get rid of the uneccessary clones of RC by passing it as reference
//TODO: write vscode plugin for language support
//TODO: treat Strings as Lists of characters to get list functionality on them
//TODO: To make the language more useful: add GC, add doubles, add user defined types, add os
//interaction layer, add tail call optimization, type inferance, lexical scoping
#[cfg(not(windows))]
extern crate editline;
extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate itertools;

mod ast;
mod env;
mod lists;
mod logic;
mod math;
mod parser;
mod strings;

use ast::Ast;
use env::Env;
use std::collections::VecDeque;
use std::rc::Rc;
use std::cell::RefCell;

const OP_EVAL: &str = "eval";
const OP_DEF: &str = "def";
const OP_PUT: &str = "let";
const OP_LAMBDA: &str = "\\";

lazy_static! {
  static ref RESERVED_WORDS: Vec<&'static str> = {
    vec![
      math::OP_MIN,
      math::OP_MAX,
      math::OP_PLUS,
      math::OP_MINUS,
      math::OP_EXP,
      math::OP_MULT,
      math::OP_DIV,
      math::OP_REM,
      lists::OP_LIST,
      lists::OP_JOIN,
      lists::OP_HEAD,
      lists::OP_TAIL,
      strings::LOAD,
      strings::PRINT,
      strings::ERROR,
      logic::OP_EQ,
      logic::OP_IF,
      logic::OP_NE,
      logic::OP_GE,
      logic::OP_LE,
      logic::OP_LT,
      logic::OP_GT,
      logic::TRUE,
      logic::FALSE,
      OP_EVAL,
      OP_DEF,
      OP_LAMBDA,
      OP_PUT,
    ]
  };
}

type OwnlispResult = Result<Ast, failure::Error>;
type LFunction = fn(VecDeque<Ast>, &Rc<RefCell<Env>>) -> OwnlispResult;

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

fn evaluate_eval(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  let mut args = args;
  if args.len() != 1 {
    bail!("Eval only works on a single Q-Expression as argument.")
  } else {
    let qexpr = evaluate(
      args.pop_front().expect("We checked for size."),
      env
    )?;
    //unpack the QExpression
    if let Ast::QExpression(list) = qexpr {
      evaluate(Ast::SExpression(list), env)
    } else {
      bail!("Eval only works with QExpression as argument.")
    }
  }
}

fn evaluate_lambda(
  args: VecDeque<Ast>,
  env: &Rc<RefCell<Env>>,
) -> OwnlispResult {
  //Check that we have two arguments and both are QExpressions
  if args.len() != 2 {
    bail!("Lambda needs 2 QExpressions as arguments.");
  }
  let mut qexprs = args
    .into_iter()
    .map(|expr| match expr {
      Ast::QExpression(qexpr) => Ok(qexpr),
      _ => Err(format_err!("Lambda only works with QExpressions!")),
    })
    .collect::<Result<VecDeque<_>, _>>()?;

  //Check That we only have Symbols in first argument list
  let formals = qexprs
    .pop_front()
    .expect("Checked len.")
    .into_iter()
    .map(|symbol| match symbol {
      Ast::Symbol(sym) => Ok(sym),
      _ => Err(format_err!("Cannot define non-symbol")),
    });
  let formals = formals.collect::<Result<VecDeque<_>, _>>()?;
  let func_env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(env)))));
  Ok(Ast::Function(
    func_env,
    formals,
    qexprs.pop_front().expect("Checked for length"),
  ))
}

enum VarType {
  Global,
  Local,
}

fn evaluate_var(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>, var_type: VarType) -> OwnlispResult {
  let mut args = args;
  match args.pop_front() {
    None => bail!("The def function needs two arguments!"),
    Some(Ast::QExpression(names)) => {
      let mut names_str: VecDeque<_> = names
        .into_iter()
        .map(|ast| match ast {
          Ast::Symbol(sym) => Ok(sym),
          _ => Err(format_err!("Function def can not define non-symbol!")),
        })
        .collect::<Result<_, _>>()?;
      if args.len() != names_str.len() {
        bail!("Function def can only define the same number of symbols to values.")
      }
      if names_str
        .iter()
        .any(|name| RESERVED_WORDS.iter().any(|reserved| reserved == name))
      {
        bail!("Redefinition of reserved keyword is not possible!")
      }
      for (name, arg) in names_str.into_iter().zip(args.into_iter()) {
        match var_type {
          VarType::Global => env.borrow_mut().put_global(name.to_owned(), arg),
          VarType::Local => env.borrow_mut().put_local(name.to_owned(), arg),
        } 
      }
      Ok(Ast::EmptyProgram)
    }
    Some(_) => bail!("Function def passed incorrect type!"),
  }
}

fn evaluate_def(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  evaluate_var(args, env, VarType::Global)
}

fn evaluate_put(args: VecDeque<Ast>, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  evaluate_var(args, env, VarType::Local)
}

fn get_variadic_part(formals: &mut VecDeque<String>) -> Result<String, failure::Error> {
  let rest_formal = match formals.pop_front() {
    Some(formal) => formal,
    None => bail!("Function format invalid. Symbol ':' not followed by a single symbol!"),
  };
  if !formals.is_empty() {
    bail!("More than one symbol after ':'!")
  }
  Ok(rest_formal)
}

fn evaluate_fun(
  env: Rc<RefCell<Env>>,
  formals: VecDeque<String>,
  arguments: VecDeque<Ast>,
  body: VecDeque<Ast>,
) -> OwnlispResult {
  let mut arguments = arguments;
  let mut formals = formals;

  while !arguments.is_empty() {
    let formal = match formals.pop_front() {
      Some(formal) => formal,
      None => bail!("Function passed too many arguments"),
    };

    //Special case for variadic parameters
    if formal == ":" {
      let rest_formal = get_variadic_part(&mut formals)?;
      let args = lists::list(arguments, &env)?;
      env.borrow_mut().put_local(rest_formal, args);
      break;
    //normal assignment
    } else {
      let arg = arguments.pop_front().expect("Arguments was not empty.");
      env.borrow_mut().put_local(formal, arg);
    }
  }

  //We have only the varidic stuff left so bind an empty list
  if !formals.is_empty() && formals[0] == ":" {
    let _sym = formals.pop_front();
    let rest_formal = get_variadic_part(&mut formals)?;
    env.borrow_mut().put_local(rest_formal, Ast::QExpression(VecDeque::new()));
  }

  if formals.is_empty() {
    //evaluate the body
    let body = Ast::SExpression(body);
    evaluate(body, &env)
  } else {
    Ok(Ast::Function(env, formals, body))
  }
}

fn evaluate_sexpression(ast: Ast, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  let sexp = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("called it wrong!"),
  };
  //Evaluate all arguments and if we have any errors bail out
  let mut sexp: VecDeque<_> = sexp
    .into_iter()
    .map(|exp| evaluate(exp, env))
    .collect::<Result<_, _>>()?;
  if sexp.len() == 1 {
    Ok(sexp.pop_front().expect("We checked the length!"))
  } else {
    match sexp.pop_front() {
      //Our arguments are empty so we evaluate to an empty expression
      None => Ok(Ast::EmptyProgram),
      //we have a function at the first position
      Some(Ast::Builtin(fun)) => fun(sexp, env),
      Some(Ast::Function(env, formals, body)) => {
        evaluate_fun(env, formals, sexp, body)
      }

      Some(_) => bail!("The first element in a SExpression needs to be a function."),
    }
  }
}

fn evaluate(program: Ast, env: &Rc<RefCell<Env>>) -> OwnlispResult {
  match program {
    Ast::Symbol(sym) => {
      //TODO: crashes because of double borrow!
      if let Some(ast) = env.borrow_mut().get(&sym) {
        Ok(ast)
      } else {
        bail!("Unbound symbol '{}'!", sym)
      }
    }
    Ast::SExpression(_) => evaluate_sexpression(program, env),
    //don't do anything for Other types of programs
    _ => Ok(program),
  }
}

fn main() {
  println!("Ownlisp version 0.0.7");
  println!("Press Ctrl+c to Exit\n");

  let env = Rc::new(RefCell::new(Env::new(None)));
  env.borrow_mut().add_builtins();
  while let Some(line) = editline::readline("Ownlisp>") {
    let line = line.trim();
    editline::add_history(&line);
    if line == "exit" {
      break;
    }
    match parser::parse_to_ast(line) {
      Ok(mut ast) => {
        let evaluated = evaluate(ast, &env);
        match evaluated {
          Ok(result) => println!("{}", result),
          Err(err) => println!("{}", err),
        }
      }
      Err(e) => println!("Syntax error: {}", e),
    }
  }
}
