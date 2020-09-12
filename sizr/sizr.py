#!/usr/bin/python3

"""
Parser prototype for Sizr transform language
"""

# TODO: pep8

import os, sys
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

class IsCaptureErr(Exception): pass
class IsNestingOpErr(Exception): pass
class IsEoiErr(Exception): pass
class IsTransformOpErr(Exception): pass
class RejectedParseErr(Exception): pass

def skipLeadingSpace(ctx: ParseCtx):
    while ctx.loc < len(ctx.src) and ctx.src[ctx.loc].isspace():
        ctx.loc += 1

# TODO: start using this over .find and preprocess
def nextTokenEndOffset(ctx: ParseCtx) -> int:
    whitespace_pattern = re.compile(r' \t\n')
    space_match = whitespace_pattern.search(ctx.remaining_src)
    return space_match.start() if space_match is not None else len(ctx.remaining_src)

def parseValue(ctx: ParseCtx):
    remaining_src = ctx.src[ctx.loc:]
    is_quote = remaining_src[0] == '"'
    is_num = remaining_src[0].isdigit()
    if is_quote:
        unescaped_quote_pattern = re.compile(r'(?<!\\)"')
        end_quote_offset = unescaped_quote_pattern.search(remaining_src[1:]).end() + 1
        ctx.loc += end_quote_offset
        skipLeadingSpace(ctx)
        # NOTE: when switching to ctx.remaining_src this won't work anymore
        return remaining_src[1:end_quote_offset-1]
    elif is_num:
        leading_num_pattern = re.compile(r'^[1-9]\d*(\.\d+)?')
        number_end = leading_num_pattern.search(remaining_src).end()
        number_sentence = remaining_src[:number_end]
        ctx.loc += number_end
        skipLeadingSpace(ctx)
        return float(number_sentence)
    else:
        next_space_offset = remaining_src.find(' ')
        ctx.loc += next_space_offset
        skipLeadingSpace(ctx)
        # NOTE: when switching to ctx.remaining_src this won't work anymore
        return remaining_src[:next_space_offset]

def isCapture(ctx: ParseCtx) -> bool:
    return bool(ctx.remaining_src) and ctx.remaining_src[0] == '$'

def parseCapture(ctx: ParseCtx):
    if not ctx.remaining_src: raise RejectedParseErr()
    remaining_src = ctx.src[ctx.loc:]
    is_regex_capture = remaining_src[:2] == '$/'
    is_named_capture = remaining_src[1:2].isalpha()
    is_anonymous_capture = remaining_src[0] == '$'
    if is_regex_capture:
        unescaped_slash_pattern = re.compile(r'(?<!\\)/')
        end_slash_offset = unescaped_slash_pattern.search(remaining_src[2:]).end() + 2
        regex_src = remaining_src[2:end_slash_offset]
        ctx.loc += end_slash_offset
        skipLeadingSpace(ctx)
        return CaptureExpr(re.compile(regex_src))
    elif is_named_capture:
        next_space_offset = remaining_src.find(' ')
        name = remaining_src[1:next_space_offset]
        ctx.loc += next_space_offset
        skipLeadingSpace(ctx)
        # TODO: escape regex metachars and/or disallow some chars
        return CaptureExpr(capture_any, name)
    elif is_anonymous_capture:
        ctx.loc += 1
        skipLeadingSpace(ctx)
        return CaptureExpr()
    else: raise RejectedParse()

def isNestingOp(ctx: ParseCtx) -> bool:
    remaining_src = ctx.src[ctx.loc:]
    # none of these intersect right now
    ops = ['.', '(', '<', '{', '[', ';', ',', '#', '@', ':']
    return any(map(lambda op: remaining_src.startswith(op), ops))

def parseNestingOp(ctx: ParseCtx) -> bool:
    # proof need check ahead and raise behind
    if isTransformOp(ctx): raise IsTransformOpErr()
    if not ctx.remaining_src: raise IsEoiErr()
    op = ctx.remaining_src[0]
    ctx.loc += 1 # nesting ops are same size
    skipLeadingSpace(ctx)
    return op

def parseIdentifier(ctx: ParseCtx) -> str:
    if not ctx.remaining_src: raise RejectedParseErr("expected an identifier")
    identifier_pattern = re.compile(r'[a-zA-Z_][a-zA-Z0-9_]*')
    match = identifier_pattern.search(ctx.remaining_src)
    identifier = ctx.remaining_src[match.start():match.end()]
    ctx.loc += match.end()
    skipLeadingSpace(ctx)
    return identifier

# TODO: follow python iterator convention
# i.e. let ctx have like an 'iter_parse_scope_props' method to do this repeatedly
def parseScopeProp(ctx: ParseCtx) -> ScopeProp:
    token_end = nextTokenEndOffset(ctx)
    if_boolean_sentence = ctx.remaining_src[:token_end]
    is_boolean_no_prop = ctx.remaining_src[0] == '!'
    if is_boolean_no_prop:
        ctx.loc += len(if_boolean_sentence)
        skipLeadingSpace(ctx)
        return ScopeProp(if_boolean_sentence[1:], False)
    is_specific_value_prop = '=' in if_boolean_sentence
    is_boolean_yes_prop = not is_specific_value_prop
    if is_boolean_yes_prop:
        ctx.loc += len(if_boolean_sentence)
        skipLeadingSpace(ctx)
        return ScopeProp(if_boolean_sentence)
    elif is_specific_value_prop:
        value_delim_index = ctx.remaining_src.find('=')
        key = ctx.remaining_src[:value_delim_index]
        ctx.loc += value_delim_index + 1
        skipLeadingSpace(ctx)
        value = parseValue(ctx)
        return ScopeProp(key, value)

def isScopeProp(ctx: ParseCtx) -> bool:
    if not ctx.remaining_src: return False
    firstchar = ctx.remaining_src[0]
    return firstchar == '!' or firstchar.isalpha()

def parseScopeExpr(ctx: ParseCtx) -> ScopeExpr:
    expr = ScopeExpr()
    while isScopeProp(ctx):
        prop = parseScopeProp(ctx)
        expr.properties.append(prop)
    if isCapture(ctx):
        expr.capture = parseCapture(ctx)
    else:
        # FIXME: confusing logic
        last_prop = expr.properties.pop()
        identifier = last_prop.key
        expr.capture = CaptureExpr(re.compile(identifier))
    if isNestingOp(ctx):
        expr.nesting_op = parseNestingOp(ctx)
    return expr

def isQuery(ctx: ParseCtx) -> bool:
    return isScopeProp(ctx) or isCapture(ctx)

isScopeExpr = isQuery

def parseQuery(ctx: ParseCtx) -> Query:
    query = Query()
    # while not isTransformOp(ctx):
    while isScopeExpr(ctx):
        expr = parseScopeExpr(ctx)
        print(expr)
        query.nested_scopes.append(expr)
    return query

def isTransformOp(ctx: ParseCtx):
    ops = ['>>>', '>>!'] # probably ought to make these referenceable variables in upper scope
    return any(map(lambda op: ctx.remaining_src.startswith(op), ops))

def parseTransformOp(ctx: ParseCtx):
    op = ctx.remaining_src[:3]
    ctx.loc += 3 # transform ops are same size
    skipLeadingSpace(ctx)
    return op

def parseTransform(src: str) -> Transform:
    ctx = ParseCtx(src)
    selector = assertion = destructive = None
    if isQuery(ctx):
        selector = parseQuery(ctx)
    if isTransformOp(ctx): # assert isTransformOp
        destructive = parseTransformOp(ctx) == ">>!"
    if isQuery(ctx):
        assertion = parseQuery(ctx)
    assert selector or assertion, "a transform command must have either a selector or an assertion"
    return Transform(selector, assertion, destructive)

def main():
    try:
        while True:
            command = input('sizr> ')
            parsed = parseTransform(command)
            print(parsed)
    except KeyboardInterrupt:
        print()

if __name__ == '__main__': main()
