use super::{
    evaluate_def, evaluate_eval, evaluate_lambda, evaluate_put, OP_DEF, OP_EVAL, OP_LAMBDA, OP_PUT,
};
use super::{lists, logic, math, strings, Ast};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


pub(crate) struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    map: HashMap<String, Ast>,
}

impl Env {
    pub(crate) fn new(parent: Option<Rc<RefCell<Env>>>) -> Env {
        Env {
            parent: parent,
            map: HashMap::new(),
        }
    }

    pub(crate) fn put_global(&mut self, key: String, value: Ast) {
        match self.parent.is_some() {
            true => {
                let mut parent = self.parent.clone();
                while let Some(env) = parent {
                    if env.borrow().parent.is_none() {
                        env.borrow_mut().put_local(key, value);
                        break;
                    }
                    parent = env.borrow().parent.clone();
                }
            }
            false => self.put_local(key, value),
        }
    }

    pub(crate) fn put_local(&mut self, key: String, value: Ast) {
        self.map.insert(key, value);
    }

    //Recursively walk up all the parents and look for the entry
    pub(crate) fn get(&self, key: &String) -> Option<Ast> {
        if let Some(ast) = self.map.get(key) {
            Some(ast.clone())
        } else {
            match self.parent {
                None => None,
                Some(ref rc_env) => rc_env.borrow().get(key).clone(),
            }
        }
    }

    pub(crate) fn add_builtins(&mut self) {
        //List functions
        self.put_local(lists::OP_LIST.to_owned(), Ast::Builtin(lists::list));
        self.put_local(lists::OP_JOIN.to_owned(), Ast::Builtin(lists::join));
        self.put_local(lists::OP_HEAD.to_owned(), Ast::Builtin(lists::head));
        self.put_local(lists::OP_TAIL.to_owned(), Ast::Builtin(lists::tail));

        //evaluate function
        self.put_local(OP_EVAL.to_owned(), Ast::Builtin(evaluate_eval));

        //Mathematical Functions
        self.put_local(math::OP_PLUS.to_owned(), Ast::Builtin(math::plus));
        self.put_local(math::OP_MINUS.to_owned(), Ast::Builtin(math::minus));
        self.put_local(math::OP_MULT.to_owned(), Ast::Builtin(math::mult));
        self.put_local(math::OP_DIV.to_owned(), Ast::Builtin(math::div));
        self.put_local(math::OP_EXP.to_owned(), Ast::Builtin(math::exp));
        self.put_local(math::OP_REM.to_owned(), Ast::Builtin(math::rem));
        self.put_local(math::OP_MIN.to_owned(), Ast::Builtin(math::min));
        self.put_local(math::OP_MAX.to_owned(), Ast::Builtin(math::max));

        //Order functions for numbers
        self.put_local(logic::OP_LE.to_owned(), Ast::Builtin(logic::lesser_equal));
        self.put_local(logic::OP_GE.to_owned(), Ast::Builtin(logic::greater_equal));
        self.put_local(logic::OP_LT.to_owned(), Ast::Builtin(logic::lesser_then));
        self.put_local(logic::OP_GT.to_owned(), Ast::Builtin(logic::greater_then));

        //Equality
        self.put_local(logic::OP_EQ.to_owned(), Ast::Builtin(logic::equal));
        self.put_local(logic::OP_NE.to_owned(), Ast::Builtin(logic::not_equal));

        //If
        self.put_local(logic::OP_IF.to_owned(), Ast::Builtin(logic::evaluate_if));

        //Constants
        self.put_local(logic::TRUE.to_owned(), Ast::Bool(true));
        self.put_local(logic::FALSE.to_owned(), Ast::Bool(false));

        //Def function
        self.put_local(OP_DEF.to_owned(), Ast::Builtin(evaluate_def));
        self.put_local(OP_PUT.to_owned(), Ast::Builtin(evaluate_put));

        //Func function
        self.put_local(OP_LAMBDA.to_owned(), Ast::Builtin(evaluate_lambda));

        //String functions
        self.put_local(
            strings::LOAD.to_owned(),
            Ast::Builtin(strings::load_ownlisp),
        );
        self.put_local(strings::PRINT.to_owned(), Ast::Builtin(strings::print));
        self.put_local(strings::ERROR.to_owned(), Ast::Builtin(strings::error));
    }
}
