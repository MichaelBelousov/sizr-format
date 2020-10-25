use super::*;

#[test]
fn test_parse_full_command() {
assert_eq!(
        parse_command("class $c . !func $/log_.*/ >>! range=5.02 item"),
        Ast::Transform{
            selector: Some(Box::new(Ast::Query{
                path: vec![
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: Some("c"),
                            pattern: None,
                        })),
                        nester: Some(NestingType::Member),
                        props: {
                            let mut map = HashMap::new();
                            map.insert("class", PropValue::Boolean(true));
                            map
                        }
                    }),
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: None,
                            pattern: Some(Regex::new("log_.*").unwrap())
                        })),
                        nester: None,
                        props: {
                            let mut map = HashMap::new();
                            map.insert("func", PropValue::Boolean(false));
                            map
                        }
                    })
                ]
            })),
            r#type: TransformType::Replacing,
            assertor: Some(Box::new(Ast::Query{
                path: vec![
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: Some("item"),
                            pattern: Some(Regex::new("item").unwrap())
                        })),
                        nester: None,
                        props: {
                            let mut map = HashMap::new();
                            map.insert("range", PropValue::Real(5.02));
                            map
                        }
                    })
                ]
            })),
        }
    );
}

#[test]
fn test_parse_args() {
assert_eq!(
        parse_command("func f ( , arg1 , arg2 >>>"),
        Ast::Transform{
            selector: Some(Box::new(Ast::Query{
                path: vec![
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: Some("f"),
                            pattern: Some(Regex::new("f").unwrap()),
                        })),
                        nester: Some(NestingType::Arg),
                        props: {
                            let mut map = HashMap::new();
                            map.insert("func", PropValue::Boolean(true));
                            map
                        }
                    }),
                    Box::new(Ast::Elem{
                        capture: None,
                        nester: Some(NestingType::Next),
                        props: HashMap::new()
                    }),
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: Some("arg1"),
                            pattern: Some(Regex::new("arg1").unwrap())
                        })),
                        nester: Some(NestingType::Next),
                        props: HashMap::new()
                    }),
                    Box::new(Ast::Elem{
                        capture: Some(Box::new(Ast::Capture{
                            name: Some("arg2"),
                            pattern: Some(Regex::new("arg2").unwrap())
                        })),
                        nester: None,
                        props: HashMap::new()
                    })
                ]
            })),
            r#type: TransformType::Additive,
            assertor: None
        }
    );
}

