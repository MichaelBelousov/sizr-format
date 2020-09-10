import os, sys
import re

class ParseCtx:
    def __init__(self, src: str, loc: int = 0):
        self.src = src
        self.loc = loc
    __str__ = lambda s: f'<ParseCtx|loc: {s.loc}, src: ""{s.src}""">'
    __repr__ = __str__

class ScopeProp: # TODO: make these namespaces, or mypy dict interfaces?
    def __init__(self, key: str, val = True):
        self.key = key
        self.val = val
    __str__ = lambda s: f'<ScopeProp|{s.key}={s.val}>'
    __repr__ = __str__

class ScopeExpr:
    def __init__(self):
        self.name = None
        self.properties: list(ScopeProp) = []

class IsCaptureNotScopePropErr(Exception):
    pass

def skipLeadingSpace(parse_ctx: ParseCtx):
    while parse_ctx.loc < len(parse_ctx.src) and parse_ctx.src[parse_ctx.loc].isspace():
        parse_ctx.loc += 1

def parseValue(parse_ctx: ParseCtx):
    remaining_src = parse_ctx.src[parse_ctx.loc:]
    is_quote = remaining_src[0] == '"'
    is_num = remaining_src[0].isdigit()
    if is_quote:
        unescaped_quote_pattern = re.compile(r'(?<!\\)"')
        end_quote_offset = unescaped_quote_pattern.search(remaining_src[1:]).end()
        parse_ctx.loc += end_quote_offset
        skipLeadingSpace(parse_ctx)
        # NOTE: when switching to parse_ctx.remaining_src this won't work anymore
        return remaining_src[1:end_quote_offset]
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
    pass

def parseScopeProp(parse_ctx: ParseCtx) -> ScopeProp:
    # add remaining_src getter to parse_ctx
    remaining_src = parse_ctx.src[parse_ctx.loc:]
    # TODO: move out to reusable `at_capture` matcher
    is_capture = remaining_src[0] == '$'
    if is_capture:
        raise IsCaptureNotScopePropErr()
    next_space_index = remaining_src.find(' ')
    if (next_space_index == -1): raise Exception("Unexpected end of prop") # FIXME
    if_boolean_sentence = remaining_src[:next_space_index]
    is_boolean_no_prop = remaining_src[0] == '!'
    if is_boolean_no_prop:
        return ScopeProp(if_boolean_sentence[1:], False)
    is_specific_value_prop = '=' in if_boolean_sentence
    is_boolean_yes_prop = not is_specific_value_prop
    if is_boolean_yes_prop:
        return ScopeProp(if_boolean_sentence)
    elif is_specific_value_prop:
        value_delim_index = remaining_src.find('=')
        key = remaining_src[:value_delim_index]
        parse_ctx.loc += value_delim_index + 1
        value = parseValue(parse_ctx)
        return ScopeProp(key, value)

def parseScopeExpr(parse_ctx: ParseCtx) -> ScopeExpr:
    return parseScopeProp(parse_ctx)

def parseQuery(src: str):
    return parseScopeExpr(ParseCtx(src))

def preprocessQuery(query: str):
    """simple preprocess for simpler handling logic"""
    # TODO: remove the need for this preprocessing pass
    return query.replace('\t', '').replace('\n', '')

def main():
    try:
        while True:
            query = input('sizr> ')
            query = preprocessQuery(query)
            parsed = parseQuery(query)
            print(parsed)
    except KeyboardInterrupt:
        print()

if __name__ == '__main__':
    main()
