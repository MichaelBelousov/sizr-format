
//#[macro_use]
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::fs;
use std::io::{self, Read};
//use std::collections::{BTreeMap, HashMap};
//use std::vec::Vec;

pub mod parser;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(std::string::String),
    Bool(bool)
    //Mapping(HashMap<Value, Value>))
    //List(Vec<Value>))
}

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer);
    parser::parse_text(&buffer);
}
