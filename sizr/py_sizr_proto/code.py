"""
sizr language code, currently slow and interpreted, hopefully eventually JITed
(won't be a python prototype at that point)
"""

# TODO: pep8

import re
from typing import List, Optional


pattern_any = re.compile('')


class CaptureExpr:
    def __init__(self, pattern=pattern_any, name: Optional[str] = None, literal: Optional[str] = None):
        self.pattern = pattern
        self.name = name  # name to get the capture by
        self.literal = literal  # set if the capture is not a wildcard or regex
    __repr__ = __str__ = lambda s: f'<Capture|name={s.name},pattern={s.pattern}>'


class ScopeExpr:
    def __init__(self, nesting_op: Optional[str] = None):
        self.capture = CaptureExpr()
        self.nesting_op = nesting_op
        self.properties = {}
    __repr__ = __str__ = lambda s: f'<ScopeExpr|capture={s.capture},props={repr(s.properties)}>'


class Query:
    """
    also known as selector or assertion when contextually on one side
    of a transform
    """

    def __init__(self, nested_scopes: Optional[List[ScopeExpr]] = None):
        self.nested_scopes = nested_scopes or []
    __repr__ = __str__ = lambda s: f'<Query|scopes={s.nested_scopes}>'


class ElementCapture:
    references = set()


class Transform:
    def __init__(self, selector: Optional[Query] = None, assertion: Optional[Query] = None, destructive=False):
        self.selector = selector or Query()
        self.assertion = assertion or Query()
        self.destructive = destructive
        self.capture_per_ref = {}
        self._findRefs()
    __repr__ = __str__ = lambda s: f'<Transform{"!" if s.destructive else ""}|selector={s.selector},assertion={s.assertion}>'

    def _findRefs(self):
        """prepare capture_per_ref cache"""
        for scope in self.selector.nested_scopes:
            pass
