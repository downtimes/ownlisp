use super::{
    evaluate_def, evaluate_eq, evaluate_eval, evaluate_if, evaluate_lambda, evaluate_ne,
    evaluate_put, FALSE, OP_DEF, OP_EQ, OP_EVAL, OP_IF, OP_LAMBDA, OP_NE, OP_PUT, TRUE,
};
use super::{lists, math, Ast, strings};
use std::collections::HashMap;

pub(crate) type EnvId = usize;

pub(crate) struct Env {
    parent_id: Option<EnvId>,
    map: HashMap<String, Ast>,
}

pub(crate) struct Environments {
    last_id: usize,
    envs: HashMap<EnvId, Env>,
}

impl Environments {
    pub(crate) fn new() -> Environments {
        let mut env = HashMap::new();
        env.insert(
            0,
            Env {
                parent_id: None,
                map: HashMap::new(),
            },
        );
        let mut envs = Environments {
            last_id: 0,
            envs: env,
        };
        envs.add_builtins();
        envs
    }

    pub(crate) fn create_env(&mut self) -> EnvId {
        self.last_id += 1;
        let new_id = self.last_id;
        self.envs.insert(
            new_id,
            Env {
                parent_id: None,
                map: HashMap::new(),
            },
        );
        new_id
    }

    // fn delete_env(&mut self, id: EnvId) {
    //   let _dont_care = self.envs.remove(&id);
    // }

    pub(crate) fn set_parent(&mut self, current: EnvId, parent: EnvId) {
        if !self.envs.contains_key(&current) {
            panic!("Tried to access non existent environment!");
        }
        self.envs
            .entry(current)
            .and_modify(|env| env.parent_id = Some(parent));
    }

    pub(crate) fn put_global(&mut self, key: String, value: Ast) {
        self.envs.entry(0).and_modify(|env| {
            env.map.insert(key, value);
        });
    }

    pub(crate) fn put_local(&mut self, env_id: EnvId, key: String, value: Ast) {
        if !self.envs.contains_key(&env_id) {
            panic!("Tried to put into non existent environment!");
        }
        self.envs.entry(env_id).and_modify(|env| {
            env.map.insert(key, value);
        });
    }

    pub(crate) fn get(&mut self, active_env: EnvId, key: &String) -> Option<Ast> {
        let mut active_env = active_env;
        //loop through all parents
        while let None = self.envs[&active_env].map.get(key) {
            match self.envs[&active_env].parent_id {
                None => return None,
                Some(id) => active_env = id,
            }
        }
        //TODO: extremely unnice. Work on overhaul of environment system!
        let val = self
            .envs
            .get_mut(&active_env)
            .expect("checked")
            .map
            .remove(key)
            .expect("checked");
        let res = Some(val.clone(self));
        self.put_local(active_env, key.clone(), val);
        res
    }

    fn add_builtins(&mut self) {
        //List functions
        self.put_global(lists::OP_LIST.to_owned(), Ast::Builtin(lists::list));
        self.put_global(lists::OP_JOIN.to_owned(), Ast::Builtin(lists::join));
        self.put_global(lists::OP_HEAD.to_owned(), Ast::Builtin(lists::head));
        self.put_global(lists::OP_TAIL.to_owned(), Ast::Builtin(lists::tail));

        //evaluate function
        self.put_global(OP_EVAL.to_owned(), Ast::Builtin(evaluate_eval));

        //Mathematical Functions
        self.put_global(math::OP_PLUS.to_owned(), Ast::Builtin(math::plus));
        self.put_global(math::OP_MINUS.to_owned(), Ast::Builtin(math::minus));
        self.put_global(math::OP_MULT.to_owned(), Ast::Builtin(math::mult));
        self.put_global(math::OP_DIV.to_owned(), Ast::Builtin(math::div));
        self.put_global(math::OP_EXP.to_owned(), Ast::Builtin(math::exp));
        self.put_global(math::OP_REM.to_owned(), Ast::Builtin(math::rem));
        self.put_global(math::OP_MIN.to_owned(), Ast::Builtin(math::min));
        self.put_global(math::OP_MAX.to_owned(), Ast::Builtin(math::max));

        //Order functions for numbers
        self.put_global(math::OP_LE.to_owned(), Ast::Builtin(math::le));
        self.put_global(math::OP_GE.to_owned(), Ast::Builtin(math::ge));
        self.put_global(math::OP_LT.to_owned(), Ast::Builtin(math::lt));
        self.put_global(math::OP_GT.to_owned(), Ast::Builtin(math::gt));

        //Equality
        self.put_global(OP_EQ.to_owned(), Ast::Builtin(evaluate_eq));
        self.put_global(OP_NE.to_owned(), Ast::Builtin(evaluate_ne));

        //If
        self.put_global(OP_IF.to_owned(), Ast::Builtin(evaluate_if));

        //Constants
        self.put_global(TRUE.to_owned(), Ast::Bool(true));
        self.put_global(FALSE.to_owned(), Ast::Bool(false));

        //Def function
        self.put_global(OP_DEF.to_owned(), Ast::Builtin(evaluate_def));
        self.put_global(OP_PUT.to_owned(), Ast::Builtin(evaluate_put));

        //Func function
        self.put_global(OP_LAMBDA.to_owned(), Ast::Builtin(evaluate_lambda));

        //String functions
        self.put_global(strings::LOAD.to_owned(), Ast::Builtin(strings::load_ownlisp));
        self.put_global(strings::PRINT.to_owned(), Ast::Builtin(strings::print));
        self.put_global(strings::ERROR.to_owned(), Ast::Builtin(strings::error));
    }
}
