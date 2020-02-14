
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct LParser;

fn main() {
    let parsed = LParser::parse(Rule::field, "-273.15");
    println!("{:?}", parsed);
    let failed = LParser::parse(Rule::field, "not a number!");
    println!("{:?}", failed);
}
