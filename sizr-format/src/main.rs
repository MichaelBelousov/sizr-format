
extern crate pest;
#[macro_use] extern crate pest_derive;
#[macro_use] extern crate lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::*;
use pest::Parser;

lazy_static! {
    static ref prec_climber: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::{Left, Right};
        use Operator as Op;

        PrecClimber::new(vec![
            Op::new(AND, Left)
            | Op::new(OR, Left)
            | Op::new(XOR, Left),
            Op::new(GT, Left)
            | Op::new(GTE, Left)
            | Op::new(EQ, Left)
            | Op::new(NEQ, Left)
            | Op::new(LTE, Left)
            | Op::new(LT, Left),
            Op::new(PLUS, Left)
            | Op::new(MINUS, Left),
            Op::new(MULT, Left)
            | Op::new(DIV, Left)
            | Op::new(INTDIV, Left)
            | Op::new(MOD, Left),
            Op::new(POW, Right)
        ])
    };
}

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct FormatDescParser;

use std::fs;
use std::collections::HashMap;
use std::vec::Vec;

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
    write_commands: Vec<WriteCommand>,
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

fn eval(expr: Pairs<Rule>) -> Value {
    prec_climber.climb(
        expr,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::integer => Value::Number(pair.as_str().parse::<f64>().unwrap()),
            Rule::quote => Value::String(pair.into_inner().as_str().to_string()),
            Rule::expr => eval(pair.into_inner()),
            /*
            Rule::regex
            Rule::var
            Rule::"(" ~ expr ~ ")"
            Rule::lambda
            */
            _ => unreachable!()
        },
        |l: Value, op: Pair<Rule>, r: Value| match (&l, op.as_rule(), &r) {
            (Value::Number(l), Rule::LT, Value::Number(r))
                => Value::Bool(*l < *r),
            (Value::String(l), Rule::LT, Value::String(r))
                => Value::Bool(*l < *r),
            (Value::Bool(l), Rule::LT, Value::Bool(r))
                => Value::Bool(!*l && *r),
            (_, Rule::LT, _)
                => panic!("can't compare {:#?} and {:#?}", l, r),
            _   => unreachable!()
        }
    )
}

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
    for cmd in &format.write_commands {
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

fn main() {
    let _ctx = ParseContext {
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
    println!("{:#?}", file);
}
