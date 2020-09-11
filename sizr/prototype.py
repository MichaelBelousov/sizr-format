#!/usr/bin/python3

"""
Parser prototype for Sizr transform language
"""

# TODO: pep8

import os, sys
import re

class EOIableStr(str):
    def __getitem__(self, start = 0, end = None, step = 1)
        if end is None and step == 1 and start >= len(self): 
            raise IsEoiErr()
        else return super()[start:end:step]

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
    def __init__(self, matcher = capture_any):
        self.matcher = matcher
    __repr__ = __str__ = lambda s: f'<Capture|{s.matcher}>'

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

def skipLeadingSpace(parse_ctx: ParseCtx):
    while parse_ctx.loc < len(parse_ctx.src) and parse_ctx.src[parse_ctx.loc].isspace():
        parse_ctx.loc += 1

# TODO: start using this over .find and preprocess
def nextWhiteSpaceOffset(ctx: ParseCtx) -> int:
    whitespace_pattern = re.compile(r' \t\n')
    return whitespace_pattern.search(ctx.remaining_src).start()

def parseValue(parse_ctx: ParseCtx):
    remaining_src = parse_ctx.src[parse_ctx.loc:]
    is_quote = remaining_src[0] == '"'
    is_num = remaining_src[0].isdigit()
    if is_quote:
        unescaped_quote_pattern = re.compile(r'(?<!\\)"')
        end_quote_offset = unescaped_quote_pattern.search(remaining_src[1:]).end() + 1
        parse_ctx.loc += end_quote_offset
        skipLeadingSpace(parse_ctx)
        # NOTE: when switching to parse_ctx.remaining_src this won't work anymore
        return remaining_src[1:end_quote_offset-1]
    elif is_num:
        leading_num_pattern = re.compile(r'^[1-9]\d*(\.\d+)?')
        number_end = leading_num_pattern.search(remaining_src).end()
        number_sentence = remaining_src[:number_end]
        parse_ctx.loc += number_end
        skipLeadingSpace(parse_ctx)
        return float(number_sentence)
    else:
        next_space_offset = remaining_src.find(' ')
        parse_ctx.loc += next_space_offset
        skipLeadingSpace(parse_ctx)
        # NOTE: when switching to parse_ctx.remaining_src this won't work anymore
        return remaining_src[:next_space_offset]

def parseCapture(parse_ctx: ParseCtx):
    remaining_src = parse_ctx.src[parse_ctx.loc:]
    is_regex_capture = remaining_src[:2] == '$/'
    is_named_capture = remaining_src[1:2].isalpha()
    if is_regex_capture:
        unescaped_slash_pattern = re.compile(r'(?<!\\)/')
        end_slash_offset = unescaped_slash_pattern.search(remaining_src[2:]).end() + 2
        regex_src = remaining_src[2:end_slash_offset]
        parse_ctx.loc += end_slash_offset
        skipLeadingSpace(parse_ctx)
        return CaptureExpr(re.compile(regex_src))
    elif is_named_capture:
        next_space_offset = remaining_src.find(' ')
        name = remaining_src[1:next_space_offset]
        parse_ctx.loc += next_space_offset
        skipLeadingSpace(parse_ctx)
        # TODO: escape regex metachars and/or disallow some chars
        return CaptureExpr(re.compile(name))
    else:
        parse_ctx.loc += 1
        skipLeadingSpace(parse_ctx)
        return CaptureExpr()

def isNestingOp(parse_ctx: ParseCtx) -> bool:
    remaining_src = parse_ctx.src[parse_ctx.loc:]
    # none of these intersect right now
    ops = ['.', '(', '<', '{', '[', ';', ',', '#', '@', ':']
    return any(map(lambda op: remaining_src.startswith(op), ops))

def parseNestingOp(ctx: ParseCtx) -> bool:
    if isTransformOp(ctx):
        print('is transform op!')
        raise IsTransformOpErr()
    op = ctx.remaining_src[0]
    ctx.loc += 1 # nesting ops are same size
    skipLeadingSpace(ctx)
    return op

# TODO: follow python iterator convention,
# let parse_ctx have like an 'iter_parse_scope_props' method to do this repeatedly
def parseScopeProp(parse_ctx: ParseCtx) -> ScopeProp:
    # TODO: remove alias
    remaining_src = parse_ctx.remaining_src
    # TODO: move out to reusable `at_capture` matcher
    is_capture = remaining_src[0] == '$'
    if is_capture: raise IsCaptureErr()
    if isTransformOp(parse_ctx): raise IsTransformOpErr()
    if isNestingOp(parse_ctx): raise IsNestingOpErr()
    next_space_index = remaining_src.find(' ') # TODO: change to findNextSpaceOffset
    if (next_space_index == -1): raise IsEoiErr()
    if_boolean_sentence = remaining_src[:next_space_index]
    is_boolean_no_prop = remaining_src[0] == '!'
    if is_boolean_no_prop:
        parse_ctx.loc += len(if_boolean_sentence)
        skipLeadingSpace(parse_ctx)
        return ScopeProp(if_boolean_sentence[1:], False)
    is_specific_value_prop = '=' in if_boolean_sentence
    is_boolean_yes_prop = not is_specific_value_prop
    if is_boolean_yes_prop:
        parse_ctx.loc += len(if_boolean_sentence)
        skipLeadingSpace(parse_ctx)
        return ScopeProp(if_boolean_sentence)
    elif is_specific_value_prop:
        value_delim_index = remaining_src.find('=')
        key = remaining_src[:value_delim_index]
        parse_ctx.loc += value_delim_index + 1
        skipLeadingSpace(parse_ctx)
        value = parseValue(parse_ctx)
        return ScopeProp(key, value)

def parseScopeExpr(parse_ctx: ParseCtx) -> ScopeExpr:
    expr = ScopeExpr()
    had_explicit_capture = False
    try:
        while True:
            prop = parseScopeProp(parse_ctx)
            print(prop)
            expr.properties.append(prop)
    except IsCaptureErr:
        had_explicit_capture = True
        expr.capture = parseCapture(parse_ctx)
    except IsNestingOpErr:
        expr.nesting_op = parseNestingOp(parse_ctx)
    if had_explicit_capture:
        try:
            expr.nesting_op = parseNestingOp(parse_ctx)
        except IsTransformOpErr:
            print('is transform op 2')
            pass
    return expr

def parseQuery(ctx: ParseCtx) -> Query:
    query = Query()
    try:
        while True:
            expr = parseScopeExpr(ctx)
            print(expr)
            query.nested_scopes.append(expr)
    except IsTransformOpErr: pass
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
    try:
        selector = parseQuery(ctx)
    except IsTransformOpErr: pass
    destructive = parseTransformOp(ctx) == ">>!"
    try:
        assertion = parseQuery(ctx)
    except IsEoiErr: pass
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

if __name__ == '__main__':
    main()
