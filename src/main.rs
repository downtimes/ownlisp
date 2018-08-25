//TODO: Errors at the moment are pretty useless. We definitely need to improve
//our error reporting so people can actually fix their broken programs
#[cfg(not(windows))]
extern crate editline;

extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;

use pest::Parser;
use std::collections::HashMap;
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
const OP_DEF: &str = "def";
const OP_LAMBDA: &str = "\\";

lazy_static! {
  static ref RESERVED_WORDS: Vec<&'static str> = {
    vec![
      OP_MIN, OP_MAX, OP_PLUS, OP_MINUS, OP_EXP, OP_MULT, OP_DIV, OP_REM, OP_LIST, OP_HEAD,
      OP_TAIL, OP_JOIN, OP_EVAL, OP_DEF, OP_LAMBDA,
    ]
  };
}

type EnvId = usize;

#[derive(Clone)]
struct Env {
  parent_id: Option<EnvId>,
  map: HashMap<String, Ast>,
}

struct Environments {
  envs: Vec<Env>,
}

impl Environments {
  fn new() -> Environments {
    Environments {
      envs: vec![Env {
        parent_id: None,
        map: HashMap::new(),
      }],
    }
  }

  fn create_env(&mut self) -> EnvId {
    self.envs.push(Env {
      parent_id: None,
      map: HashMap::new(),
    });
    self.envs.len() - 1
  }

  fn set_parent(&mut self, env: EnvId, parent: EnvId) {
    self.envs[env].parent_id = Some(parent);
  }

  fn put_global(&mut self, key: String, value: Ast) {
    self.envs[0].map.insert(key, value);
  }

  fn put_local(&mut self, env: EnvId, key: String, value: Ast) {
    self.envs[env].map.insert(key, value);
  }

  fn get(&self, active_env: EnvId, key: &String) -> Option<&Ast> {
    let mut active_env = active_env;
    //loop through all parents
    while let None = self.envs[active_env].map.get(key) {
      match self.envs[active_env].parent_id {
        None => return None,
        Some(id) => active_env = id,
      }
    }
    self.envs[active_env].map.get(key)
  }
}

type OwnlispResult = Result<Ast, failure::Error>;
type LFunction = fn(Ast, &mut Environments, active_env: EnvId) -> OwnlispResult;

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

#[derive(Clone)]
enum Ast {
  Number(i64),
  Builtin(LFunction),
  Function(EnvId, VecDeque<String>, VecDeque<Ast>),
  Symbol(String),
  SExpression(VecDeque<Ast>),
  QExpression(VecDeque<Ast>),
  EmptyProgram,
}

impl std::fmt::Debug for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match *self {
      Ast::Builtin(_) => write!(f, "Builtin(<builtin>)"),
      _ => write!(f, "{:?}", self),
    }
  }
}

impl std::fmt::Display for Ast {
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

fn apply_op(op: &str, x: i64, y: i64) -> Result<i64, failure::Error> {
  match op {
    OP_MINUS => Ok(x - y),
    OP_PLUS => Ok(x + y),
    OP_MULT => Ok(x * y),
    OP_DIV => {
      if y == 0 {
        bail!("Attempted division by 0.")
      } else {
        Ok(x / y)
      }
    }
    OP_REM => Ok(x % y),
    //TODO: Check for if the conversation here is safe.
    OP_EXP => Ok(x.pow(y as u32)),
    OP_MIN => Ok(std::cmp::min(x, y)),
    OP_MAX => Ok(std::cmp::max(x, y)),
    _ => panic!("We tried to apply a qexpression operator on a number"),
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

      Rule::symbol => Ast::Symbol(pair.as_str().to_owned()),

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

macro_rules! create_evaluate_math {
  ($name:ident, $op:expr) => {
    fn $name(ast: Ast, _env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
      if let Ast::SExpression(sexp) = ast {
        evaluate_math_builtin($op, sexp)
      } else {
        panic!("Wrong call!")
      }
    }
  };
}

create_evaluate_math!(evaluate_mult, OP_MULT);
create_evaluate_math!(evaluate_div, OP_DIV);
create_evaluate_math!(evaluate_plus, OP_PLUS);
create_evaluate_math!(evaluate_minus, OP_MINUS);
create_evaluate_math!(evaluate_rem, OP_REM);
create_evaluate_math!(evaluate_exp, OP_EXP);
create_evaluate_math!(evaluate_min, OP_MIN);
create_evaluate_math!(evaluate_max, OP_MAX);

fn evaluate_math_builtin(op: &str, sexp: VecDeque<Ast>) -> OwnlispResult {
  //fast bailout if we don't only have numbers as operands
  let sexp: Vec<_> = sexp
    .into_iter()
    .map(|operand| match operand {
      Ast::Number(x) => Ok(x),
      _ => Err(format_err!("Math functions only work with numbers!")),
    })
    .collect::<Result<_, _>>()?;

  let mut operands = sexp.into_iter().peekable();
  let first_operand = operands.next();
  match first_operand {
    Some(operand) => {
      let mut total = operand;
      //we only had one operand so check if it's an unary operator
      if operands.peek().is_none() {
        match op {
          OP_MINUS => Ok(Ast::Number(-total)),
          OP_MIN | OP_MAX => Ok(Ast::Number(total)),
          _ => bail!("The function {} is not an unary function.", op),
        }
      //We have many operands
      } else {
        for val in operands {
          total = apply_op(op, total, val)?;
        }
        Ok(Ast::Number(total))
      }
    }
    None => bail!("Functions need arguments to work on."),
  }
}

fn evaluate_eval(ast: Ast, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!"),
  };
  if expr.len() != 1 {
    bail!("Eval only works on a single Q-Expression as argument.")
  } else {
    let qexpr = evaluate(
      expr.pop_front().expect("We checked for size."),
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

fn evaluate_join(ast: Ast, _env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  let expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!"),
  };

  let mut qlists: VecDeque<_> = expr
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

fn evaluate_head(ast: Ast, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!"),
  };
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr, env, active_env)?;
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

fn evaluate_tail(ast: Ast, env: &mut Environments, active_env: EnvId) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!"),
  };
  if expr.len() == 1 {
    //Get the first element of the inner QExpression.
    let first_expr = expr.pop_front().expect("We checked the size.");
    let qexpr = evaluate(first_expr, env, active_env)?;
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

fn evaluate_list(ast: Ast, _env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  if let Ast::SExpression(exprs) = ast {
    Ok(Ast::QExpression(exprs))
  } else {
    panic!("This function can only be called with SExpressions");
  }
}

fn evaluate_lambda(ast: Ast, env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  if let Ast::SExpression(sexpr) = ast {
    //Check that we have two arguments and both are QExpressions
    if sexpr.len() != 2 {
      bail!("Lambda needs 2 QExpressions as arguments.");
    }
    let mut qexprs = sexpr
      .into_iter()
      .map(|expr| match expr {
        Ast::QExpression(qexpr) => Ok(qexpr),
        _ => Err(format_err!("Lambda only works with QExpressions!")),
      })
      .collect::<Result<VecDeque<_>, _>>()?;

    //Check That we only have Symbols in first argument list
    let formals = qexprs.pop_front().expect("Checked len.").into_iter().map(
      |symbol| match symbol {
        Ast::Symbol(sym) => Ok(sym),
        _ => Err(format_err!("Cannot define non-symbol")),
      },
    );
    let formals = formals.collect::<Result<VecDeque<_>, _>>()?;
    Ok(Ast::Function(
      env.create_env(),
      formals,
      qexprs.pop_front().expect("Checked for length"),
    ))
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_def(ast: Ast, env: &mut Environments, _active_env: EnvId) -> OwnlispResult {
  if let Ast::SExpression(mut sexp) = ast {
    match sexp.pop_front() {
      None => bail!("The def function needs two arguments!"),
      Some(Ast::QExpression(names)) => {
        let mut names_str: VecDeque<_> = names
          .into_iter()
          .map(|ast| match ast {
            Ast::Symbol(sym) => Ok(sym),
            _ => Err(format_err!("Function def can not define non-symbol!")),
          })
          .collect::<Result<_, _>>()?;
        if sexp.len() != names_str.len() {
          bail!("Function def can only define the same number of symbols to values.")
        }
        if names_str
          .iter()
          .any(|name| RESERVED_WORDS.iter().any(|reserved| reserved == name))
        {
          bail!("Redefinition of reserved keyword is not possible!")
        }
        for (i, name) in names_str.into_iter().enumerate() {
          env.put_global(name.to_owned(), sexp[i].clone());
        }
        Ok(Ast::EmptyProgram)
      }
      Some(_) => bail!("Function def passed incorrect type!"),
    }
  } else {
    panic!("Wrong call!")
  }
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
  active_env: EnvId,
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
      let args = evaluate_list(Ast::SExpression(arguments), env, active_env)?;
      env.put_local(func_env, rest_formal, args);
      break;
    //normal assignment
    } else {
      env.put_local(
        func_env,
        formal,
        arguments.pop_front().expect("Arguments was not empty."),
      );
    }
  }

  //We have only the varidic stuff left so bind an empty list
  if !formals.is_empty() && formals[0] == ":" {
    let _sym = formals.pop_front();
    let rest_formal = get_variadic_part(&mut formals)?;
    env.put_local(func_env, rest_formal, Ast::QExpression(VecDeque::new()));
  }

  if formals.is_empty() {
    //Adjust parent environment
    env.set_parent(func_env, active_env);

    //evaluate the body
    evaluate(Ast::SExpression(body), env, func_env)
  } else {
    let new_body = replace_sym_in_body(func_env, env, body);
    Ok(Ast::Function(func_env, formals, new_body))
  }
}

fn replace_sym_in_body(active_env: EnvId, env: &Environments, body: VecDeque<Ast>) -> VecDeque<Ast> {
  let mut res = VecDeque::new();
  for bod in body.into_iter() {
    match bod {
      Ast::Symbol(sym) => {
        let new_node = match env.get(active_env, &sym) {
          None => Ast::Symbol(sym),
          Some(ast) => ast.clone(),
        };
        res.push_back(new_node);
      }
      Ast::QExpression(qexpr) => {
        res.push_back(Ast::QExpression(replace_sym_in_body(active_env, env, qexpr)));
      }
      Ast::SExpression(sexpr) => {
        res.push_back(Ast::SExpression(replace_sym_in_body(active_env, env, sexpr)));
      }
      _ => res.push_back(bod),
    }
  }
  res
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
      //Our arguments are empty so we evaluate to the empty program
      None => Ok(Ast::EmptyProgram),
      //we have a function at the first position
      Some(Ast::Builtin(fun)) => fun(Ast::SExpression(sexp), env, active_env),
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
        Ok(ast.clone())
      } else {
        bail!("Unbound symbol!")
      }
    }
    Ast::SExpression(_) => evaluate_sexpression(program, environment, active_env),
    //don't do anything for Other types of programs
    _ => Ok(program),
  }
}

fn add_builtins(env: &mut Environments) {
  //List functions
  env.put_global(OP_LIST.to_owned(), Ast::Builtin(evaluate_list));
  env.put_global(OP_HEAD.to_owned(), Ast::Builtin(evaluate_head));
  env.put_global(OP_TAIL.to_owned(), Ast::Builtin(evaluate_tail));
  env.put_global(OP_EVAL.to_owned(), Ast::Builtin(evaluate_eval));
  env.put_global(OP_JOIN.to_owned(), Ast::Builtin(evaluate_join));

  //Mathematical Functions
  env.put_global(OP_PLUS.to_owned(), Ast::Builtin(evaluate_plus));
  env.put_global(OP_MINUS.to_owned(), Ast::Builtin(evaluate_minus));
  env.put_global(OP_MULT.to_owned(), Ast::Builtin(evaluate_mult));
  env.put_global(OP_DIV.to_owned(), Ast::Builtin(evaluate_div));
  env.put_global(OP_EXP.to_owned(), Ast::Builtin(evaluate_exp));
  env.put_global(OP_REM.to_owned(), Ast::Builtin(evaluate_rem));
  env.put_global(OP_MIN.to_owned(), Ast::Builtin(evaluate_min));
  env.put_global(OP_MAX.to_owned(), Ast::Builtin(evaluate_max));

  //Def function
  env.put_global(OP_DEF.to_owned(), Ast::Builtin(evaluate_def));
  //Func function
  env.put_global(OP_LAMBDA.to_owned(), Ast::Builtin(evaluate_lambda));
}

fn main() {
  println!("Ownlisp version 0.0.5");
  println!("Press Ctrl+c to Exit\n");

  let mut environment = Environments::new();
  add_builtins(&mut environment);

  while let Some(line) = editline::readline("Ownlisp>") {
    let line = line.trim();
    editline::add_history(&line);
    if line == "exit" {
      break;
    }
    let parsed = OwnlispParser::parse(Rule::program, line);
    if let Ok(mut pairs) = parsed {
      let program = build_custom_ast(pairs.next().unwrap());
      //println!("{}", program);
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
