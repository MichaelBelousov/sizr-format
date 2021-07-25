#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::io::{self, Read};
//use std::fs;
//use std::collections::{BTreeMap, HashMap};
//use std::vec::Vec;

//mod vm;
mod parser;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let ctx = parser::ParseContext::new(&buffer);
    let result = parser::parse_text(&ctx);
    println!("result: {:#?}", result);
    Ok(())
}
