"""
sizr language code, currently slow and interpreted, hopefully eventually JITed
(won't be a python prototype at that point)
"""

# TODO: pep8

import re
from .util import FrozenDict
from typing import List, Optional, Set, Dict
import libcst as cst


pattern_any = re.compile('')


class CaptureExpr:
    """an expression describing a capture"""

    def __init__(self, pattern=pattern_any, name: Optional[str] = None, literal: Optional[str] = None):
        self.pattern = pattern
        self.name = name  # name to get the capture by
        # TODO: rename
        self.literal = literal  # set if the capture is not a wildcard or regex

    __repr__ = __str__ = lambda s: f'<{type(s).__name__}|$/{s.pattern.pattern}/{s.name if s.name is not None else ""}>'

    def __eq__(self, r):
        return self.pattern == r.pattern and self.name == r.name


class CapturedElement(CaptureExpr):
    """realized ast node from a capture expression"""

    def __init__(self, base: CaptureExpr, node: cst.CSTNode):
        super().__init__(base.pattern, base.name, base.literal)
        self.node = node
        self.references: Set[cst.CSTNode] = set()

    def __eq__(self, r):
        return self.pattern == r.pattern and self.name == r.name


class ScopeExpr:
    def __init__(self, nesting_op: Optional[str] = None):
        self.capture = CaptureExpr()
        self.nesting_op = nesting_op
        self.properties = {}

    def __eq__(self, r):
        return (self.capture.name == r.capture.name
                and self.nesting_op == r.nesting_op
                and self.properties == r.properties)

    def __hash__(self):
        return hash((self.capture.name,
                     self.nesting_op,
                     # maybe use frozenset instead of sorted
                     tuple(sorted(self.properties.items()))))

    @property
    def _props_str(self) -> str:
        return ' '.join(
            f'{k}={v}' if not isinstance(v, bool)
            else f'{"" if v else "!"}{k}'
            for k, v, in self.properties.items())

    __repr__ = __str__ = lambda s: f'<{type(s).__name__}|{s._props_str} {s.capture} {s.nesting_op}>'


class CapturedScope(ScopeExpr):
    def __init__(self, base: ScopeExpr, node: cst.CSTNode):
        super().__init__(base.nesting_op)
        self.capture = CapturedElement(base.capture, node)
        self.properties = FrozenDict(base.properties)


class Query:
    """can be a selector or assertion when contextually on one side of a transform"""

    def __init__(self, nested_scopes: Optional[List[ScopeExpr]] = None):
        self.nested_scopes = nested_scopes or []

    __repr__ = __str__ = lambda s: f'<{type(s).__name__}|scopes={s.nested_scopes}>'


class CapturedSelection(Query):
    """query with captured ast nodes"""

    def __init__(self, base: Query, nodes: List[cst.CSTNode]):
        super().__init__([CapturedScope(s, n)
                          for s, n in zip(base.nested_scopes, nodes)])


# TODO: derived from import discovery, will need facility for direct references, like
# C++ includes and
class ProgramContext:
    def __init__(self):
        self.modules = []
        self.module_graph = {}  # TODO: pick a graph data module


class TransformExpr:
    def __init__(self, selector: Optional[Query] = None, assertion: Optional[Query] = None, destructive=False):
        self.selector = selector or Query()
        self.assertion = assertion or Query()
        self.destructive = destructive

    __repr__ = __str__ = lambda s: f'''<{type(s).__name__}{
        '!' if s.destructive else ''
    }|selector={s.selector},assertion={s.assertion}>'''


class TransformContext(TransformExpr):
    # XXX: maybe replace captured_nodes with direct captured_selection
    def __init__(self, base: TransformExpr, captured_nodes: List[cst.CSTNode]):
        super().__init__(CapturedSelection(base.selector,
                                           captured_nodes), base.assertion, base.destructive)
        self.captures_by_name = self._init_captures_by_name()

    # XXX: More evidence of horrible naming that needs to be fixed
    def _init_captures_by_name(self) -> Dict[ScopeExpr, ScopeExpr]:
        # TODO: nested_scopes need to be selected_elems_path or something
        # but vscode+rope isn't working well so ideally I can use sizr to fix it eventually
        captured_elems_by_name = {s.capture.name: s
                                  for s in self.selector.nested_scopes}
        return {ref.capture.name: captured_elems_by_name[ref.capture.name]
                for ref in self.assertion.nested_scopes
                if ref.capture.name in captured_elems_by_name}
