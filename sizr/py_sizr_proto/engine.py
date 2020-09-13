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
    # FIXME: fix CaptureExpr vs Capture[Ref?] name
    __repr__ = __str__ = lambda s: f'<CaptureRef|name={s.name},node=\n{astor.to_source(s.node)}>'


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
def astNodeFromAssertion(assertion: Query, match: SelectionMatch) -> ast.AST:
    if not assertion.nested_scopes:
        return
    cur_scope, *next_scopes = assertion.nested_scopes
    cur_capture, *next_captures = match.captures
    name = cur_scope.capture.literal or cur_capture.node.name
    next_assertion = Query()
    next_assertion.nested_scopes = next_scopes
    inner = astNodeFromAssertion(next_assertion, SelectionMatch(next_captures))
    if 'class' in cur_scope.properties and cur_scope.properties['class']:
        result = ast.ClassDef()
        result.name = name
        result.bases = []
        result.keywords = []
        result.body = [ast.Pass() if inner is None else inner]
        result.decorator_list = []
        return result
    if 'func' in cur_scope.properties and cur_scope.properties['func']:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        result = ast.AsyncFunctionDef() if cur_scope.properties.get(
            'async', False) else ast.FunctionDef()
        result.name = name
        result.args = ast.arguments()
        result.args.args = []
        result.args.vararg = None
        result.args.kwonlyargs = []
        result.args.kw_default = []
        result.args.kwarg = None
        result.args.defaults = []
        result.body = [ast.Pass() if inner is None else inner]
        result.decorator_list = []
        result.returns = None
        return result
    raise Exception("Could not determine a node type from the name properties")


def dictKeysAndValues(d): return d.keys(), d.values()


def select(root: ast.AST, selector: Query) -> List[SelectionMatch]:
    selected: List[SelectionMatch] = []

    # TODO: dont root search at global scope, that's not the original design
    #       I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
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
                next_captures = [*captures,
                                 Capture(node, cur_scope.capture.name)]
                if rest_scopes:
                    search(node, rest_scopes,
                           cur_scope.nesting_op, next_captures)
                else:
                    selected.append(SelectionMatch(next_captures))

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


def destroy_selection(py_ast: ast.AST, matches: Iterator[SelectionMatch] = {}) -> ast.AST:
    """ remove the selected nodes from the AST, for destructive queries """

    class DestroySelection(ast.NodeTransformer):
        def visit(self, node: ast.AST):
            target = find(lambda m: astEq(
                m.captures[-1].node, node), matches)
            # TODO: create a module tree from scratch for the assertion and merge the trees
            if target is not None:
                return None
            else:
                return super().visit(node)

    class FixEmptyBodies(ast.NodeTransformer):
        def visit(self, node: ast.AST):
            if (hasattr(node, 'body')
                        and isinstance(node.body, list)
                        and not node.body
                    ):
                node.body.append(ast.Pass())
            return super().visit(node)

    post_destroy_tree = DestroySelection().visit(py_ast)
    bodies_fixed_tree = FixEmptyBodies().visit(post_destroy_tree)
    fixed_tree = ast.fix_missing_locations(bodies_fixed_tree)
    return fixed_tree


def assert_(py_ast: ast.AST, assertion: Query, matches: Optional[Iterator[SelectionMatch]]) -> ast.AST:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen
    """
    if matches is None:
        matches = set()

    class Transformer(ast.NodeTransformer):
        def visit(self, node):
            target = find(lambda m: astEq(
                m.captures[0].node, node), matches)
            # TODO: create a module tree from scratch for the assertion and merge the trees
            if target is not None:
                transformed_node = astNodeFromAssertion(assertion, target)
                # FIXME: needs to work with argument scopes as well...
                # need to crawl the tree and mark all roots that need to be merged
                # with the assertion tree
                return mergeAsts(node, transformed_node)
            else:
                return super().visit(node)

    transformed_tree = Transformer().visit(py_ast)
    fixed_tree = ast.fix_missing_locations(transformed_tree)

    return fixed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = ast.parse(src)
    selection = None
    if transform.selector:
        selection = select(py_ast, transform.selector)
        print('Selected:')
        print("#########################################")
        for s in selection:
            for c in s.captures:
                print(astor.to_source(c.node))
                print("#########################################")
    if transform.destructive:
        py_ast = destroy_selection(py_ast, selection)
    if transform.assertion:
        py_ast = assert_(py_ast, transform.assertion, selection)
    print('Transformed:')
    print("#########################################")
    result = astor.to_source(py_ast)
    print(result)
    return result
