
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct FormatDescParser;

use std::fs;
use std::collections::HashMap;
use std::vector::Vec;
use std::io::{self, Read};
use std::env;

pub enum BinExpr {
    // Comparison
    LessThan            {l: Box<Expr>, r: Box<Expr>},
    GreaterThan         {l: Box<Expr>, r: Box<Expr>},
    Equal               {l: Box<Expr>, r: Box<Expr>},
    NotEqual            {l: Box<Expr>, r: Box<Expr>},
    GreaterThanOrEqual  {l: Box<Expr>, r: Box<Expr>},
    // Arithmetic
    LessThanOrEqual     {l: Box<Expr>, r: Box<Expr>},
    Add                 {l: Box<Expr>, r: Box<Expr>},
    Sub                 {l: Box<Expr>, r: Box<Expr>},
    Mult                {l: Box<Expr>, r: Box<Expr>},
    Pow                 {l: Box<Expr>, r: Box<Expr>},
    Divide              {l: Box<Expr>, r: Box<Expr>},
    Remainder           {l: Box<Expr>, r: Box<Expr>},
    IntDivide           {l: Box<Expr>, r: Box<Expr>},
    // Logical
    Or                  {l: Box<Expr>, r: Box<Expr>},
    And                 {l: Box<Expr>, r: Box<Expr>},
    Xor                 {l: Box<Expr>, r: Box<Expr>},
}

pub enum UnaryExpr {
    Negate              {e: Box<Expr>},
    Complement          {e: Box<Expr>},
    Parenthesized       {e: Box<Expr>}
}

pub enum Expr {
    Binary(BinExpr),
    Unary(UnaryExpr)
}

pub enum WriteCommand {
    Literal(String),
    Breakpoint,
    Cond { expr: Expr
         , if_: Box<WriteCommand>
         , else_: Box<WriteCommand>
         }
}

//make serializable for caching
struct NodeFormat{
    // TODO: use inkwell to JIT the format rule
    writeCommands: Vec<WriteCommand>,
}

struct Node {
    type_: str,
}

static mut node_formats: HashMap<String, &NodeFormat> = HashMap::with_capacity(100);

enum Value {
    Number(f64),
    String(std::string::String),
    Bool_(bool)
}

type Context = HashMap<String, Value>;

// built in fluster style cast operator?
// with the types: f, float, i, int, n: number, s: string, b:
// e.g.: `class $name $props.len:b
// no that looks ugly, should implicitly convert a decent amount...
// depends totally on idioms, but at least need to convert to boo,
// maybe use rational numbers instead of number=floats|integer?
//
// Script variable types:
// patterns:
// - literal patterns (strings!) look like "TEXT"
// - regex (PCRE) patterns look like /here?(is)[my](?=regex!)$/
// - "" is false, everything elseis true
// numbers:
// - int is subset of float, floats cannot be used for indexing
// - 0 is false, everything else is true
// boolean:
// - true or false
// mapping/list:
// - dynamic typed elements
// - empty list is false
// - can be sliced/filtered
// - index slice: `mylist[0..10]`, `mylist[0..+2..10]`,
// - lambda slice: `mylist[.static]` (choose all static members)
// lambdas:
// - arrow function: arg => `expr`, `(a1, a2) => expr`
// - property shorthand lambdas `.name`
// - lambda predicates can have set operators act on them
// - funcs[.static-f=>f.returns="double"]

fn exec_bin_op(expr: &Expr) {
    match (l, r) {
        number, number => true
        string, string => true
        bool_, bool_ => true
        _, _ => panic!()
    }
    match expr {
        EExpr::LessThan {l, r} => l < r,
        EExpr::GreaterThan => l > r,
        EExpr::Equal => l == r,
        EExpr::NotEqual => l != r,
        EExpr::GreaterThanOrEqual => l >= r,
        EExpr::LessThanOrEqual => l <= r,
        EExpr::Add => l + r,
        EExpr::Sub => l - r,
        EExpr::Mult => l * r,
        EExpr::Pow => l.pow(r),
        EExpr::Divide => l / r,
        EExpr::IntDivide => l / r,
        EExpr::Remainder => l % r,
        EExpr::Negate => -l,
        EExpr::Or => l | r,
        EExpr::And => l & r,
        EExpr::Xor => l ^ r,
        EExpr::Complement{e} => ~e,
    }
}

fn serialize(node: &Node, ctx: &Context) {
    let format = node_formats[node.type_];
    for cmd in format.writeCommands {
        match cmd {
            WriteCommand::Literal(s) => ctx.writes.last().append(s)
            WriteCommand::Breakpoint => ctx.writes.append(String::from(""))
            WriteCommand::Expr(e) => match e {
                EExpr::LessThan{l,r} => l < r,
                EExpr::GreaterThan{l,r} => l > r,
                EExpr::Equal{l,r} => l == r,
                EExpr::NotEqual{l,r} => l != r,
                EExpr::GreaterThanOrEqual{l,r} => l >= r,
                EExpr::LessThanOrEqual{l,r} => l <= r,
                EExpr::Add{l,r} => l + r,
                EExpr::Sub{l,r} => l - r,
                EExpr::Mult{l,r} => l * r,
                EExpr::Pow{l,r} => l.pow(r),
                EExpr::Divide{l,r} => l / r,
                EExpr::IntDivide{l,r} => l / r,
                EExpr::Remainder{l,r} => l % r,
                EExpr::Negate{l,r} => -l,
                EExpr::Or{l,r} => l | r,
                EExpr::And{l,r} => l & r,
                EExpr::Xor{l,r} => l ^ r,
                EExpr::Complement{e} => ~e,
            }

        }
    }
}

// TODO: write custom parser for performance reasons, because of the weird way
// characters will be dealt with

fn main() {

  let hellovar = FormatDescParser::parse(Rule::var, "$hello")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("VAR: {:#?}", hellovar);

  let nodebody = FormatDescParser::parse(Rule::node_body, "'''$hello'''")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("NODEBODY: {:#?}", nodebody);

  let nodedecl = FormatDescParser::parse(Rule::node_decl, r"my_rule : '''$hel'''")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("NODE_DECL: {:#?}", nodedecl);

  let cwd = env::current_dir();
  println!("{:?}", cwd.unwrap().display());


  let src_file =
    fs::read_to_string("./example.sizf")
        .expect("cannot read file");
  let file = FormatDescParser::parse(Rule::file, &src_file)
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("{:#?}", file);

  let mut buffer = String::new();
  io::stdin().read_to_string(&mut buffer);
  let input = FormatDescParser::parse(Rule::file, &buffer)
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("STDIN: {:#?}", input);
}
