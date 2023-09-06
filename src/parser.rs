use crate::ast::Ast;
use anyhow::bail;
use anyhow::Result;
use itertools::Itertools;
use pest::Parser;
use pest_derive::Parser;
use std::collections::VecDeque;

//This is done so we generate a new binary if we modify only the grammar
#[cfg(debug_assertions)]
const _GRAMMAR: &str = include_str!("ownlisp.pest");

#[derive(Parser)]
#[grammar = "ownlisp.pest"]
struct OwnlispParser;

pub(crate) fn parse_to_ast(input: &str) -> Result<Ast> {
    fn build_ast(pair: pest::iterators::Pair<Rule>) -> Ast {
        match pair.as_rule() {
            Rule::number => Ast::Number(
                pair.as_str()
                    .parse()
                    .expect("We expect to only get valid numbers here"),
            ),

            Rule::string => Ast::Str(pair.as_str().trim_matches('"').to_owned()),

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
                pair.into_inner()
                    .next()
                    .expect("Expressions always consist of one item"),
            ),

            Rule::program => {
                let pairs = pair.into_inner();
                let elems: VecDeque<_> = pairs.dropping_back(1).map(build_ast).collect();
                if elems.is_empty() {
                    Ast::EmptyProgram
                } else {
                    Ast::SExpression(elems)
                }
            }

            _ => {
                println!("{pair:?}");
                panic!("Unknown parsing rule encountered");
            }
        }
    }

    let mut pairs = match OwnlispParser::parse(Rule::program, input) {
        Ok(pairs) => pairs,
        Err(e) => bail!("{e}"),
    };

    Ok(build_ast(pairs.next().unwrap()))
}
