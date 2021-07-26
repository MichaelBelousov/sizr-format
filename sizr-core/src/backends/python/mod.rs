use std::collections::HashMap;
use pyo3::prelude::*;


use crate::code::Elem;

struct PythonElem<'a> {
    _src: &'a str,
    _start: usize
}

impl<'a> Elem<'a, ()> for PythonElem<'a> {
    fn new(_src: &'a str, _start: usize, _ctx: ()) -> Self { PythonElem{ _src, _start } }
    fn src(&self) -> &'a str { self._src }
    fn start(&self) -> usize { self._start }
}

// probably want a Backend trait with initializer for RAII
pub fn init() {
    let gil = Python::acquire_gil();
    let py = gil.python();
    let libcst = py.import("libcst").unwrap();
    let CSTNode = libcst.get("CSTNode");
}

struct PyCstNode;

fn node_matches_prop(prop: &str, node: PyCstNode) -> bool {
    match prop {
        "func" => true, //py.isinstance()
        "class" => false,
        "var" => true,
        _ => false
    }
}

//fn possible_node_classes_per_prop<'a>(prop: &'a str) -> Set<&'str> {}


