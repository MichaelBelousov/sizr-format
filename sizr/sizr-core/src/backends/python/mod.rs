
use crate::code::Elem;

struct PythonElem<'a> {
    _src: &'a str,
    _start: usize
}

impl<'a> Elem<'a> for PythonElem<'a> {
    fn new(_src: &'a str, _start: usize) -> Self { PythonElem{ _src, _start } }
    fn src(&self) -> &'a str { self._src }
    fn start(&self) -> usize { self._start }
}
