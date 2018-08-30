use super::{VecDeque, Ast, EnvId, Environments, OwnlispResult};

pub const OP_LIST: &str = "list";
pub const OP_JOIN: &str = "join";
pub const OP_HEAD: &str = "head";
pub const OP_TAIL: &str = "tail";

pub(crate) fn join(
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

pub(crate) fn list(
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

pub(crate) fn head(
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

pub(crate) fn tail(
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
