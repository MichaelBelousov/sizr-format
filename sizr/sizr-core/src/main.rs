mod parser;

//use std::fs;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let result = parser::parse_command(&buffer);
    println!("result: {:#?}", result);
    Ok(())
}
