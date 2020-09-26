"""
sizr language code, currently slow and interpreted, hopefully eventually JITed
(won't be a python prototype at that point)
"""

# TODO: pep8

import re
from typing import List, Optional


capture_any = re.compile('')


class CaptureExpr:
    def __init__(self, pattern=capture_any, name=None, literal: str or None = None):
        self.pattern = pattern
        self.name = name  # name to get the capture by
        self.literal = literal  # set if the capture is not a wildcard or regex
    __repr__ = __str__ = lambda s: f'<Capture|name={s.name},pattern={s.pattern}>'


class ScopeExpr:
    def __init__(self, nesting_op: str or None = None):
        self.capture = CaptureExpr()
        self.nesting_op = nesting_op
        self.properties = {}
    __repr__ = __str__ = lambda s: f'<ScopeExpr|capture={s.capture},props={repr(s.properties)}>'


class Query:
    """
    also known as selector or assertion when contextually on one side
    of a transform
    """

    def __init__(self, nested_scopes: List[ScopeExpr] or None = None):
        self.nested_scopes = [] if nested_scopes is None else nested_scopes
    __repr__ = __str__ = lambda s: f'<Query|scopes={s.nested_scopes}>'


class Transform:
    def __init__(self, selector: Optional[Query] = None, assertion: Query or None = None, destructive: bool = False):
        self.selector = selector
        self.assertion = assertion
        self.destructive = destructive
    __repr__ = __str__ = lambda s: f'<Transform|selector={s.selector},assertion={s.assertion}>'
