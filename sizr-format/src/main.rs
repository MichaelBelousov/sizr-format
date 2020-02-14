
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "format.pest"]
pub struct FormatDescParser;

use std::fs;
use std::collections::HashMap;
use std::io::{self, Read};

fn main() {

  let hellovar = FormatDescParser::parse(Rule::var, "$hello")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("VAR: {:#?}", hellovar);

  let nodebody = FormatDescParser::parse(Rule::node_body, "'''$hello'''")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("NODEBODY: {:#?}", nodebody);

  let nodedecl = FormatDescParser::parse(Rule::node_decl, "my_rule: '''$hello'''")
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("NODE_DECL: {:#?}", nodedecl);

  /*
  let src_file =
    fs::read_to_string("example.sizf")
        .expect("cannot read file");
  let file = FormatDescParser::parse(Rule::file, &src_file)
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("{:#?}", file);
  */

  let mut buffer = String::new();
  io::stdin().read_to_string(&mut buffer);
  let input = FormatDescParser::parse(Rule::file, &buffer)
      .expect("unsuccessful parse")
      .next()
      .unwrap();
  println!("STDIN: {:#?}", input);
}
