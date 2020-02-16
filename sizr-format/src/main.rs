
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct FormatDescParser;

use std::fs;
use std::collections::HashMap;
use std::vec::Vec;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
pub enum BinExpr {
    // Comparison
    LessThan            {l: Box<Expr>, r: Box<Expr>},
    /*
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
    */
}

#[derive(Debug)]
pub enum UnaryExpr {
    Negate              {e: Box<Expr>},
    /*
    LogicalNegate       {e: Box<Expr>},
    Complement          {e: Box<Expr>},
    Parenthesize        {e: Box<Expr>},
    */
}

// might need to optimize the alignment on nested enum...?
#[derive(Debug)]
pub enum Expr {
    Binary(BinExpr),
    Unary(UnaryExpr),
    Value(Value),
}

#[derive(Debug)]
pub enum WriteCommand {
    Literal(String),
    Breakpoint,
    Cond { expr: Expr
         , if_: Box<WriteCommand>
         , else_: Box<WriteCommand>
         }
}

//make serializable for caching
#[derive(Debug)]
struct NodeFormat{
    // TODO: use inkwell to JIT the format rule
    writeCommands: Vec<WriteCommand>,
}

#[derive(Debug)]
struct Node {
    type_: str,
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(std::string::String),
    Bool(bool)
    //Mapping(HashMap<Value, Value>))
    //List(Vec<Value>))
}

struct ParseContext {
    variables: HashMap<String, Value>,
    node_formats: HashMap<String, NodeFormat>,
}

struct WriteContext {
    writes: Vec<String>,
}

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
// - can union slices with set union (&) or commas:
// - mylist[.static,.private]
// - the "leftover slice" is [...]
// lambdas:
// - arrow function: arg => `expr`, `(a1, a2) => expr`
// - property shorthand lambdas `.name`
// - lambda predicates can have set operators act on them
// - funcs[.static-f=>f.returns="double"]
//
// example special case node:
// struct:
// '''
// struct $name {
//   #<> this is a whitespace sucking comment
//   #<> .same is a built in slice that returns either the
//   #<> .same is a built in slice that returns the whole set only
//   #<> 
//   $body[r,g,b][.same]? "$[1].type r, g, b;" #<> special case if you have those props
//   $body[...] #< remainder of body, + keep trailing whitespace! <#    
// }
// '''

impl Expr {
    fn eval(&self) -> Value {
        match self {
            // figure out why it cannot just dereference? maybe own a cached value?
            Expr::Value(v) => v.clone(),
            Expr::Binary(BinExpr::LessThan{l, r})
                => match (l.eval(), r.eval()) {
                    (Value::Number(l), Value::Number(r))
                        => Value::Bool(l < r),
                    (Value::String(l), Value::String(r))
                        => Value::Bool(l < r),
                    (Value::Bool(l), Value::Bool(r))
                        => Value::Bool(!l && r),
                    // TODO: remove debug and use display
                    _ => panic!("type error: left hand side, '{:?}'
                                 and right hand side, '{:?}', cannot be compared", l,r),
                },
            Expr::Unary(UnaryExpr::Negate{e})
                => { 
                    let v = e.eval();
                    match v {
                        Value::Number(v) => Value::Number(-v),
                        _ => panic!("type error: unary operator '{:?}' does
                                     not support argument '{:?}'.", "-", v)
                    }
                },
        }
    }
}

fn serialize(node: &Node, ctx: &ParseContext, writeCtx: &mut WriteContext) {
    let format = &ctx.node_formats[&node.type_];
    if !writeCtx.writes.is_empty() { writeCtx.writes.push(String::from("")); }
    for cmd in &format.writeCommands {
        match cmd {
            WriteCommand::Literal(s) =>
                if let Some(last) = writeCtx.writes.last_mut() {
                    last.push_str(&s);
                },
            WriteCommand::Breakpoint =>
                writeCtx.writes.push(String::from("")),
            /*
            // handle correctly recursively later
            WriteCommand::Cond{expr, if_, else_} => 
                if let Some(last) = writeCtx.writes.last_mut() {
                    last.push_str(if expr.eval() { if_ } else { else_ });
                    serialize(
                        Node { writes: Vec![if expr.eval() {if_} else {else_}] },
                        ctx,
                        writeCtx
                    );
                },
            */
            _ => ()
        }
    }
}

// TODO: write custom parser for performance reasons, because of the weird way
// characters will be dealt with

/*
fn compileFormat(parsed: typeof FormatDescParse::parse()) -> NodeFormat {
}
*/

fn main() {
    let ctx = ParseContext {
        node_formats: HashMap::with_capacity(100),
        variables: HashMap::with_capacity(10),
    };

    let src_file =
        fs::read_to_string("./example.sizf")
        .expect("cannot read file");
    let file = FormatDescParser::parse(Rule::file, &src_file)
        .expect("unsuccessful parse")
        .next()
        .unwrap();
    //println!("{:#?}", file);

    for node_decl in file.into_inner() {
        match node_decl.as_rule() {
            Rule::node_decl => {
                for write in node_decl.into_inner() {
                    match write.as_rule() {
                        Rule::var => {},
                        Rule::breakpt => {},
                        Rule::cond => {},
                        _ => unreachable!(),
                    }
                }
            },
            Rule::EOI  => (),
            _ => unreachable!(),
        }
    }


    /*
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer);
    let input = FormatDescParser::parse(Rule::file, &buffer)
    .expect("unsuccessful parse")
    .next()
    .unwrap();
    println!("STDIN: {:#?}", input);
    */
}
