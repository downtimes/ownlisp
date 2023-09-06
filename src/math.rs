use super::{OwnlispResult, VecDeque};
use crate::ast::Ast;
use crate::env::Env;
use anyhow::bail;
use anyhow::Result;
use std::cell::RefCell;
use std::cmp;
use std::rc::Rc;

pub const OP_MIN: &str = "min";
pub const OP_MAX: &str = "max";
pub const OP_PLUS: &str = "+";
pub const OP_MINUS: &str = "-";
pub const OP_EXP: &str = "^";
pub const OP_MULT: &str = "*";
pub const OP_DIV: &str = "/";
pub const OP_REM: &str = "%";

//TODO: all these functions don't care about the environment. That smells.

fn apply_op(op: &str, x: i64, y: i64) -> Result<i64> {
    match op {
        OP_MINUS => Ok(x - y),
        OP_PLUS => Ok(x + y),
        OP_MULT => Ok(x * y),
        OP_DIV => {
            if y == 0 {
                bail!("Attempted division by 0.")
            }
            Ok(x / y)
        }
        OP_REM => Ok(x % y),
        //TODO: Check for if the conversion here is safe.
        OP_EXP => Ok(x.pow(u32::try_from(y)?)),
        OP_MIN => Ok(cmp::min(x, y)),
        OP_MAX => Ok(cmp::max(x, y)),
        _ => panic!("We tried to apply a qexpression operator on a number"),
    }
}

macro_rules! create_evaluate_math {
    ($name:ident, $op:expr) => {
        pub(crate) fn $name(args: VecDeque<Ast>, _env: &Rc<RefCell<Env>>) -> OwnlispResult {
            evaluate($op, args)
        }
    };
}

create_evaluate_math!(mult, OP_MULT);
create_evaluate_math!(div, OP_DIV);
create_evaluate_math!(plus, OP_PLUS);
create_evaluate_math!(minus, OP_MINUS);
create_evaluate_math!(rem, OP_REM);
create_evaluate_math!(exp, OP_EXP);
create_evaluate_math!(min, OP_MIN);
create_evaluate_math!(max, OP_MAX);

fn evaluate(op: &str, sexp: VecDeque<Ast>) -> OwnlispResult {
    //fast bailout if we don't only have numbers as operands
    let sexp: Vec<_> = sexp
        .into_iter()
        .map(|operand| match operand {
            Ast::Number(x) => Ok(x),
            _ => bail!("Math functions only work with numbers!"),
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
