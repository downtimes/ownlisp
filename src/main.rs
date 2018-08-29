//TODO: Errors at the moment are pretty useless. We definitely need to improve
//our error reporting so people can actually fix their broken programs
//TODO: implement error printing for lisp users
//TODO: refactoring. Verschidene teile in vershieden Dateien etc
//TODO: write vscode plugin for language support
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

mod math;
mod env;
mod ast;

use pest::Parser;
use env::{Environments, EnvId};
use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use ast::Ast;

const OP_LIST: &str = "list";
const OP_JOIN: &str = "join";
const OP_EVAL: &str = "eval";
const OP_HEAD: &str = "head";
const OP_TAIL: &str = "tail";
const OP_DEF: &str = "def";
const OP_EQ: &str = "=";
const OP_NE: &str = "!=";
const OP_IF: &str = "if";
const OP_PUT: &str = "let";
const TRUE: &str = "true";
const FALSE: &str = "false";
const LOAD: &str = "load";
const PRINT: &str = "print";
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
      math::OP_GE,
      math::OP_LE,
      math::OP_LT,
      math::OP_GT,
      OP_LIST,
      OP_JOIN,
      OP_EVAL,
      OP_DEF,
      OP_LAMBDA,
      OP_EQ,
      TRUE,
      FALSE,
      OP_IF,
      OP_NE,
      LOAD,
      OP_HEAD,
      OP_TAIL,
      OP_PUT,
      PRINT,
    ]
  };
}


type OwnlispResult = Result<Ast, failure::Error>;
type LFunction = fn(VecDeque<Ast>, &mut Environments, active_env: EnvId) -> OwnlispResult;

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


fn evaluate_ne(args: VecDeque<Ast>, _env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  let eq = evaluate_eq(args, _env, _active_env);
  match eq {
    Ok(Ast::Bool(b)) => Ok(Ast::Bool(!b)),
    _ => eq,
  }
}

fn evaluate_eq(args: VecDeque<Ast>, _env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  if args.len() != 2 {
    bail!("The equality operator only works with two arguments!")
  }
  let first = &args[0];
  let second = &args[1];
  Ok(Ast::Bool(first == second))
}

fn evaluate_eval(args: VecDeque<Ast>, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  let mut args = args;
  if args.len() != 1 {
    bail!("Eval only works on a single Q-Expression as argument.")
  } else {
    let qexpr = evaluate(
      args.pop_front().expect("We checked for size."),
      env,
      active_env,
    )?;
    //unpack the QExpression
    if let Ast::QExpression(list) = qexpr {
      evaluate(Ast::SExpression(list), env, active_env)
    } else {
      bail!("Eval only works with QExpression as argument.")
    }
  }
}

fn evaluate_join(
  args: VecDeque<Ast>,
  _env: &mut Environments,
  _active_env: EnvId,
) -> OwnlispResult {
  let mut qlists: VecDeque<_> = args
    .into_iter()
    .map(|expr| match expr {
      Ast::QExpression(exp) => Ok(exp),
      _ => Err(format_err!("Join can only work on QExpressions!")),
    })
    .collect::<Result<_, _>>()?;

  match qlists.pop_front() {
    //Flatten all the QExpressions into the first one
    Some(mut res) => {
      for mut list in qlists {
        res.append(&mut list);
      }
      Ok(Ast::QExpression(res))
    }
    None => bail!("Join needs an argument to work on."),
  }
}

fn evaluate_head(
  args: VecDeque<Ast>,
  _env: &mut Environments,
  _active_env: EnvId,
) -> OwnlispResult {
  if args.len() != 1 {
    bail!("head can only work with a single QExpression!")
  }
  let mut args = args;
  if let Ast::QExpression(mut list) = args.pop_front().expect("checked") {
    let mut res = VecDeque::new();
    match list.pop_front() {
      Some(item) => res.push_back(item),
      None => {}
    }
    Ok(Ast::QExpression(res))
  } else {
    bail!("head only works on QExpressions!")
  }
}

fn evaluate_tail(
  args: VecDeque<Ast>,
  _env: &mut Environments,
  _active_env: EnvId,
) -> OwnlispResult {
  if args.len() != 1 {
    bail!("tail can only work with a single QExpressio!")
  }
  let mut args = args;
  if let Ast::QExpression(mut list) = args.pop_front().expect("checked") {
    let _dontcare = list.pop_front();
    Ok(Ast::QExpression(list))
  } else {
    bail!("tail can only work on QExpressions!")
  }
}

fn evaluate_list(
  args: VecDeque<Ast>,
  _env: &mut Environments,
  _active_env: EnvId,
) -> OwnlispResult {
  //flatten empty stuff away
  let args = args
    .into_iter()
    .filter(|arg| match arg {
      Ast::QExpression(qexpr) => !qexpr.is_empty(),
      Ast::SExpression(sexpr) => !sexpr.is_empty(),
      _ => true,
    })
    .collect();
  Ok(Ast::QExpression(args))
}

fn evaluate_lambda(
  args: VecDeque<Ast>,
  env: &mut Environments,
  _active_env: EnvId,
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
  let func_env = env.create_env();
  env.set_parent(func_env, 0);
  Ok(Ast::Function(
    func_env,
    formals,
    qexprs.pop_front().expect("Checked for length"),
  ))
}

fn evaluate_var(args: VecDeque<Ast>, env: &mut Environments, to_put: EnvId) -> OwnlispResult {
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
        env.put_local(to_put, name.to_owned(), arg);
      }
      Ok(Ast::EmptyProgram)
    }
    Some(_) => bail!("Function def passed incorrect type!"),
  }
}

fn evaluate_def(args: VecDeque<Ast>, env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  evaluate_var(args, env, 0)
}

fn evaluate_put(args: VecDeque<Ast>, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  evaluate_var(args, env, active_env)
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
  env: &mut Environments,
  parent_env: EnvId,
  func_env: EnvId,
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
      let args = evaluate_list(arguments, env, parent_env)?;
      env.put_local(func_env, rest_formal, args);
      break;
    //normal assignment
    } else {
      let arg = arguments.pop_front().expect("Arguments was not empty.");
      env.put_local(func_env, formal, arg);
    }
  }

  //We have only the varidic stuff left so bind an empty list
  if !formals.is_empty() && formals[0] == ":" {
    let _sym = formals.pop_front();
    let rest_formal = get_variadic_part(&mut formals)?;
    env.put_local(func_env, rest_formal, Ast::QExpression(VecDeque::new()));
  }

  if formals.is_empty() {
    //evaluate the body
    let body = Ast::SExpression(body);
    evaluate(body, env, func_env)
  } else {
    Ok(Ast::Function(func_env, formals, body))
  }
}

fn evaluate_if(args: VecDeque<Ast>, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  if args.len() != 3 {
    bail!("If needs three arguments!")
  }
  let mut args = args;
  if let Ast::Bool(b) = args.pop_front().expect("checked.") {
    if let Ast::QExpression(true_block) = args.pop_front().expect("checked.") {
      if let Ast::QExpression(false_block) = args.pop_front().expect("checked.") {
        if b {
          evaluate_sexpression(Ast::SExpression(true_block), env, active_env)
        } else {
          evaluate_sexpression(Ast::SExpression(false_block), env, active_env)
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

fn print_string(args: VecDeque<Ast>, _env: &mut Environments, _act_env: EnvId) -> OwnlispResult {
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

fn load_ownlisp(args: VecDeque<Ast>, env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
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

fn evaluate_sexpression(ast: Ast, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  let sexp = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("called it wrong!"),
  };
  //Evaluate all arguments and if we have any errors bail out
  let mut sexp: VecDeque<_> = sexp
    .into_iter()
    .map(|exp| evaluate(exp, env, active_env))
    .collect::<Result<_, _>>()?;
  if sexp.len() == 1 {
    Ok(sexp.pop_front().expect("We checked the length!"))
  } else {
    match sexp.pop_front() {
      //Our arguments are empty so we evaluate to an empty expression
      None => Ok(Ast::EmptyProgram),
      //we have a function at the first position
      Some(Ast::Builtin(fun)) => fun(sexp, env, active_env),
      Some(Ast::Function(env_id, formals, body)) => {
        evaluate_fun(env, active_env, env_id, formals, sexp, body)
      }

      Some(_) => bail!("The first element in a SExpression needs to be a function."),
    }
  }
}

fn evaluate(program: Ast, environment: &mut Environments, active_env: EnvId) -> OwnlispResult {
  match program {
    Ast::Symbol(sym) => {
      if let Some(ast) = environment.get(active_env, &sym) {
        Ok(ast)
      } else {
        bail!("Unbound symbol '{}'!", sym)
      }
    }
    Ast::SExpression(_) => evaluate_sexpression(program, environment, active_env),
    //don't do anything for Other types of programs
    _ => Ok(program),
  }
}


fn main() {
  println!("Ownlisp version 0.0.7");
  println!("Press Ctrl+c to Exit\n");

  let mut environment = Environments::new();
  while let Some(line) = editline::readline("Ownlisp>") {
    let line = line.trim();
    editline::add_history(&line);
    if line == "exit" {
      break;
    }
    let parsed = OwnlispParser::parse(Rule::program, line);
    if let Ok(mut pairs) = parsed {
      let program = ast::build(pairs.next().unwrap());
      let evaluated = evaluate(program, &mut environment, 0);
      match evaluated {
        Ok(result) => println!("{}", result),
        Err(err) => println!("{}", err),
      }
    } else {
      println!("Syntax error: {:?}", parsed.unwrap_err());
    }
  }
}
