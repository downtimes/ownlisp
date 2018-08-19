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

type Env = HashMap<String, Ast>;
type OwnlispResult = Result<Ast, failure::Error>;
type LFunction = fn(Ast, &mut Env) -> OwnlispResult;

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
  Function(LFunction),
  Symbol(String),
  SExpression(VecDeque<Ast>),
  QExpression(VecDeque<Ast>),
  EmptyProgram,
}

impl std::fmt::Debug for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match *self {
      Ast::Function(_) => write!(f, "Function(<function>)"),
      _ => write!(f, "{:?}", self),
    }
  }
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
      Ast::Function(_) => write!(f, "<function>"),
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

fn apply_op(op: &str, x: Ast, y: Ast) -> OwnlispResult {
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
        Ast::Symbol(pair.as_str().to_owned())
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

//TODO: It's functional but needs a lot of cleanup. Maybe some Macros just to train it
fn evaluate_mult(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_MULT, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_div(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_DIV, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_rem(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_REM, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_exp(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_EXP, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_minus(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_MINUS, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_add(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_PLUS, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_min<'a>(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_MIN, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_max(ast: Ast, env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(sexp) = ast {
    evaluate_math_builtin(OP_MAX, sexp, env)
  } else {
    panic!("Wrong call!");
  }
}

fn evaluate_math_builtin(op: &str, sexp: VecDeque<Ast>, env: &mut Env) -> OwnlispResult {
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
          _ => bail!("The function {} is not an unary function.", op),
        }
      //We have many operands
      } else {
        for val in values {
          total = apply_op(op, total, evaluate(val, env)?)?
        }
        Ok(total)
      }
    }
    None => bail!("Functions need a arguments to work on."),
  }
}

fn evaluate_eval(ast: Ast, env: &mut Env) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!")
  };
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

fn evaluate_join(ast: Ast, _env: &mut Env) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!")
  };
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

fn evaluate_head(ast: Ast, env: &mut Env) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!")
  };
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

fn evaluate_tail(ast: Ast, env: &mut Env) -> OwnlispResult {
  let mut expr = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("Wrong call!")
  };
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

fn evaluate_list(ast: Ast, _env: &mut Env) -> OwnlispResult {
  if let Ast::SExpression(exprs) = ast {
    Ok(Ast::QExpression(exprs))
  } else {
    panic!("This function can only be called with SExpressions");
  }
}

fn evaluate_sexpression(ast: Ast, env: &mut Env) -> OwnlispResult {
  let sexp = match ast {
    Ast::SExpression(sexp) => sexp,
    _ => panic!("called it wrong!")
  };
  //Evaluate all arguments and if we have any errors bail out
  let mut sexp: VecDeque<_> = sexp.into_iter().map(|exp| evaluate(exp, env)).collect::<Result<_, _>>()?;
  match sexp.pop_front() {
    //Our arguments are empty so we evaluate to the empty program
    None => Ok(Ast::EmptyProgram),
    //we have a function at the first position
    Some(Ast::Function(fun)) => {
      fun(Ast::SExpression(sexp), env)
    }

    //We have no function in the first position. That's only okay if
    //we have only one value
    Some(val) => {
      if sexp.is_empty() {
        evaluate(val, env)
      } else {
        bail!("The first element of a S-Expression needs to be a function.")
      }
    }
  }
}

fn evaluate(program: Ast, environment: &mut Env) -> OwnlispResult {
  match program {
    Ast::Function(_) => Ok(program),
    Ast::Number(_) => Ok(program),
    Ast::Symbol(sym) => {
      if let Some(ast) = environment.get(&sym) {
        Ok(ast.clone())
      } else {
        Ok(Ast::Symbol(sym))
      }
    }
    Ast::SExpression(_) => evaluate_sexpression(program, environment),
    //don't do anything for Q-Expressions
    Ast::QExpression(_) => Ok(program),
    Ast::EmptyProgram => Ok(program),
  }
}

fn add_builtins(env: &mut Env) {
  //List functions
  env.insert(OP_LIST.to_owned(), Ast::Function(evaluate_list));
  env.insert(OP_HEAD.to_owned(), Ast::Function(evaluate_head));
  env.insert(OP_TAIL.to_owned(), Ast::Function(evaluate_tail));
  env.insert(OP_EVAL.to_owned(), Ast::Function(evaluate_eval));
  env.insert(OP_JOIN.to_owned(), Ast::Function(evaluate_join));

  //Mathematical Functions
  env.insert(OP_PLUS.to_owned(), Ast::Function(evaluate_add));
  env.insert(OP_MINUS.to_owned(), Ast::Function(evaluate_minus));
  env.insert(OP_MULT.to_owned(), Ast::Function(evaluate_mult));
  env.insert(OP_DIV.to_owned(), Ast::Function(evaluate_div));
  env.insert(OP_EXP.to_owned(), Ast::Function(evaluate_exp));
  env.insert(OP_REM.to_owned(), Ast::Function(evaluate_rem));
  env.insert(OP_MIN.to_owned(), Ast::Function(evaluate_min));
  env.insert(OP_MAX.to_owned(), Ast::Function(evaluate_max));
}

fn main() {
  println!("Ownlisp version 0.0.3");
  println!("Press Ctrl+c to Exit\n");

  let mut environment = Env::new();
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
