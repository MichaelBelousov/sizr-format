
use std::fs;
use std::collections::HashMap;
use std::vec::Vec;

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
}
