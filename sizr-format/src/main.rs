
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct LParser;

fn main() {
    let test_src = r"
    my_node: '''
        $here \ $is?some $possible:node \?body
    '''
    ";
    let parsed = LParser::parse(Rule::node_body, test_src);
    println!("{:?}", parsed);
}
