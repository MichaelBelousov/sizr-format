"""
sizr language code, currently slow and interpreted, hopefully eventually JITed
(won't be a python prototype at that point)
"""

# TODO: pep8

import re

class ParseCtx:
    def __init__(self, src: str, loc: int = 0):
        self.src = src
        self.loc = loc
    __repr__ = __str__ = lambda s: f'''\
<ParseCtx
| loc={s.loc}
, src={repr(s.src)}>
, remaining_src{repr(s.remaining_src)}
>'''
    @property
    def remaining_src(self):
        return self.src[self.loc:]

class ScopeProp: # TODO: make these namespaces, or mypy dict interfaces?
    def __init__(self, key: str, val = True):
        self.key = key
        self.val = val
    __repr__ = __str__ = lambda s: f'<ScopeProp|{s.key}={repr(s.val)}>'

capture_any = re.compile('.*')

class CaptureExpr:
    def __init__(self, pattern = capture_any, name = None):
        self.pattern = pattern
        self.name = name
    __repr__ = __str__ = lambda s: f'<Capture|name={s.name},pattern={s.pattern}>'

class ScopeExpr:
    def __init__(self, nesting_op: str = None):
        self.capture = CaptureExpr()
        self.nesting_op = nesting_op
        self.properties: list(ScopeProp) = []
    __repr__ = __str__ = lambda s: f'<ScopeExpr|capture={s.capture},props={repr(s.properties)}>'

class Query: # TODO: selector is the lhs, assertion is rhs, need a unified name
    def __init__(self):
        self.nested_scopes = []
    __repr__ = __str__ = lambda s: f'<Query|scopes={s.nested_scopes}>'

class Transform:
    def __init__(self, selector: Query or None = None, assertion: Query or None = None, destructive: bool = False):
        self.selector = selector
        self.assertion = assertion
        self.destructive = destructive
    __repr__ = __str__ = lambda s: f'<Transform|selector={s.selector},assertion={s.assertion}>'
