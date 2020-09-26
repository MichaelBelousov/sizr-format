"""
sizr language code, currently slow and interpreted, hopefully eventually JITed
(won't be a python prototype at that point)
"""

# TODO: pep8

import re
from typing import List, Optional, Set, Dict
import libcst as cst


pattern_any = re.compile('')


class CaptureExpr:
    def __init__(self, pattern=pattern_any, name: Optional[str] = None, literal: Optional[str] = None):
        self.pattern = pattern
        self.name = name  # name to get the capture by
        self.literal = literal  # set if the capture is not a wildcard or regex
    __repr__ = __str__ = lambda s: f'<Capture|name={s.name},pattern={s.pattern}>'

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
                     tuple(sorted(self.properties.items()))))

    __repr__ = __str__ = lambda s: f'<ScopeExpr|capture={s.capture},props={repr(s.properties)}>'


class Query:
    """
    also known as selector or assertion when contextually on one side
    of a transform
    """

    def __init__(self, nested_scopes: Optional[List[ScopeExpr]] = None):
        self.nested_scopes = nested_scopes or []
    __repr__ = __str__ = lambda s: f'<Query|scopes={s.nested_scopes}>'


class ProgramContext:
    def __init__(self):
        self.modules = []
        self.module_graph = {}  # TODO: pick a graph data module


class ElementCapture:
    def __init__(self):
        self.references: Set[cst.CSTNode] = set()


class Transform:
    def __init__(self, selector: Optional[Query] = None, assertion: Optional[Query] = None, destructive=False):
        self.selector = selector or Query()
        self.assertion = assertion or Query()
        self.destructive = destructive
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

    __repr__ = __str__ = lambda s: f'<Transform{"!" if s.destructive else ""}|selector={s.selector},assertion={s.assertion}>'
