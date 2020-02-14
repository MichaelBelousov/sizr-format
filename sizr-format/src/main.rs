
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct LParser;

fn main() {
    let parsed = LParser::parse(Rule::node_body, "-273.15");
    println!("{:?}", parsed);
    let failed = LParser::parse(Rule::node_body, "not a number!");
    println!("{:?}", failed);
}
