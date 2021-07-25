#[macro_use]
extern crate lazy_static;
extern crate pyo3;

//use std::fs;
use std::io::{self, Read};

mod code;
mod backends {
    mod python;
}
mod parser;
//mod engine;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let result = parser::parse_command(&buffer);
    println!("result: {:#?}", result);
    Ok(())
}
