"""
AST transformation engine prototype for Sizr transform language
"""

import ast
import astor
from code import Query, Transform, ScopeExpr, capture_any
from typing import Optional, List, Set, Iterator, Dict

node_table = {
    ast.ClassDef: {
        'func': lambda n: True,
        'class': lambda n: True,
        'name': lambda node: node.name,
    },
    ast.FunctionDef: {
        'func': True,
    },
}

property_testers = {
    'func': lambda val, node: isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)),
    'class': lambda val, node: isinstance(node, ast.ClassDef),
}

nesting_op_filters = {
    '.': lambda node: ast.iter_child_nodes(node) if isinstance(node, ast.ClassDef) else (),
    '(': lambda node: ast.iter_child_nodes(node.args) if isinstance(node, ast.FunctionDef, ast.AsyncFunctionDef) else (),
    None: lambda node: ast.iter_child_nodes(node),
}


class Capture:
    def __init__(self, node: ast.AST, name: Optional[str] = None):
        self.node = node
        self.name = name


class SelectionMatch:
    CaptureType = Capture

    def __init__(self, captures: List[CaptureType]):
        self.captures = captures

    # probably want capture list to be retrievable by name,
    # perhaps abstracting over int-indexable map/dict is in order
    def getCaptureByName(self, name: str) -> CaptureType:
        first, *_ = filter(lambda c: c.name == name, self.captures)
        return first


# TODO: proof of the need to clarify a datum names, as `Element` or `ProgramElement` or `Unit` or `Name`
def astNodeFromAssertion(assertion: Query, selection: SelectionMatch) -> ast.AST:
    # FIXME: only supports last for now
    print('astNodeFromAssertion')
    print(assertion)
    expr = assertion.nested_scopes[-1]
    print(expr)
    name = assertion.nested_scopes[-1].capture.literal or selection.captures[-1].node.name
    if 'class' in expr.properties and expr.properties['class']:
        result = ast.ClassDef()
        result.name = name
        result.bases = []
        result.keywords = []
        result.body = [ast.Pass()]
        result.decorator_list = []
        print('class result')
        print(astor.to_source(result))
        return result
    if 'func' in expr.properties and expr.properties['func']:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        result = ast.AsyncFunctionDef() if expr.properties.get(
            'async', False) else ast.FunctionDef()
        result.name = name
        result.args = ast.arguments()
        result.args.args = []
        result.args.vararg = None
        result.args.kwonlyargs = []
        result.args.kw_default = []
        result.args.kwarg = None
        result.args.defaults = []
        result.body = [ast.Pass()]
        result.decorator_list = []
        result.returns = None
        print('func result')
        print(astor.to_source(result))
        return result
    raise Exception("Could not determine a node type from the name properties")


def dictKeysAndValues(d): return d.keys(), d.values()


def select(py_src: str, selector: Query) -> List[SelectionMatch]:
    selected: List[SelectionMatch] = []
    root = ast.parse(py_src)

    # TODO: dont root search at global scope, that's not the original design
    #       I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw the typing module briefly and I'm pretty sure it doesn't work this way
    def search(node: ast.AST, scopes, nesting_op: str or None = None, captures: Optional[List[ast.AST]] = None):
        if captures is None:
            captures = []
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_filters[nesting_op](node):
            # FIXME: autopep8 is making this really ugly... (or maybe I am)
            if ((cur_scope.capture == capture_any
                 or (hasattr(node, 'name')
                     and cur_scope.capture.pattern.match(node.name) is not None))
                    # TODO: abstract to literate function "matchesScopeProps"?
                    and all(map(lambda k, v: property_testers[k](v, node),
                                *dictKeysAndValues(cur_scope.properties)))):
                if rest_scopes:
                    search(node, rest_scopes, cur_scope.nesting_op,
                           [*captures, Capture(node, cur_scope.capture.name)])
                else:
                    selected.append(SelectionMatch(captures))

    search(root, selector.nested_scopes)
    return selected  # maybe should have search return the result list


# NOTE: probably will go with an IPC based architecture for enabling
# multiple language backends to communicate with the sizr engine.
# Probably ought to look at the language-server-protocol (LSP) as well


def mergeAsts(a: ast.AST, b: ast.AST) -> ast.AST:
    # FIXME: unimplemented
    # NOTE: if I choose for this to fix locations while working, I cannot
    # rely on locations for ast node value equality...
    return b


def find(func, itr: Iterator):
    """
    TODO: move to general utilities
    """
    try:
        return next(filter(func, itr))
    except:
        pass


def astEq(a: ast.AST, b: ast.AST) -> bool:
    """because it doesn't seem to work have its own value equality"""
    return ((a.col_offset, a.end_col_offset, a.lineno, a.end_lineno)
            == (b.col_offset, b.end_col_offset, b.lineno, b.end_lineno)
            )


def assert_(py_src: str, assertion: Query, selection: Optional[Set[SelectionMatch]]) -> ast.AST or str:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen
    """
    if selection is None:
        selection = {}

    class Transformer(ast.NodeTransformer):
        def visit(self, node):
            target = find(lambda s: astEq(
                s.captures[-1].node, node), selection)
            if target is not None:
                transformed_node = astNodeFromAssertion(
                    assertion, target)
                return mergeAsts(node, transformed_node)
            else:
                return super().visit(node)

    py_ast = ast.parse(py_src)
    transformed_tree = Transformer().visit(py_ast)
    fixed_tree = ast.fix_missing_locations(transformed_tree)

    result = astor.to_source(fixed_tree)
    return result

    # NOTE: default to print to stdout, take a cli arg for target file for now
