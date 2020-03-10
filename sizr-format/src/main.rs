
use std::fs;
use std::collections::HashMap;
use std::vec::Vec;

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

fn main() {
}
