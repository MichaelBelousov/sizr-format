"""
AST transformation engine prototype for Sizr transform language
"""

import ast
import astor
from typing import Optional, List, Set, Iterator, Tuple
from functools import reduce
from .code import Query, Transform, ScopeExpr, capture_any
import operator


# TODO: maybe make 'immutable'?, since hash should only be implemented on immutable objects?
class PathEquatableAst(ast.AST):
    def __init__(self, node: ast.AST, path: Tuple[(str, int)] = ()):
        self.node = node
        self.path = path

    def __eq__(self, other):
        return self.path == other.path

    def __hash__(self):
        return hash(self.path)

    def __getattr__(self, attr):
        return getattr(self.node, attr)

    @staticmethod
    def wrap(node, *rest: __init__):  # more typing abuse
        class Wrapper(PathEquatableAst):
            pass
        Wrapper.__name__ = node.__class__.__name__
        Wrapper.__name__ = node.__class__.__name__
        Wrapper.__doc__ = node.__class__.__doc__
        return Wrapper(node, *rest)


def parseAst(src: str) -> PathEquatableAst:
    """produce an ast of custom PathEquatableAst nodes"""
    root = ast.parse(src)

    # NOTE: maybe it's a bad idea to use the type syntax willynilly without knowing how it works
    # and ignoring it... a mypy experienced developer might come along and be thoroughly confused
    def wrap(node: ast.AST, path: Tuple[(str, int)] = ()):
        for attr, val in node.__dict__.items():
            if isinstance(val, ast.AST):
                setattr(node, attr, wrap(val, (*path, (attr, 0))))
            if isinstance(val, list):
                setattr(node, attr, [wrap(n, (*path, (attr, i)))
                                     for i, n in enumerate(val) if isinstance(n, ast.AST)])
        return PathEquatableAst.wrap(node, path)

    return wrap(root)


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

# TODO: descend by nesting op?
nesting_op_filters = {
    '.': lambda node: ast.iter_child_nodes(node) if isinstance(node, ast.ClassDef) else (),
    '(': lambda node: ast.iter_child_nodes(node.args) if isinstance(node, ast.FunctionDef, ast.AsyncFunctionDef) else (),
    None: lambda node: ast.iter_child_nodes(node),
}


class Capture:
    def __init__(self, node: PathEquatableAst, name: Optional[str] = None):
        self.node = node
        self.name = name
    # FIXME: fix CaptureExpr vs Capture[Ref?] naming
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
def astNodeFromAssertion(assertion: Query, match: SelectionMatch) -> PathEquatableAst:
    if not assertion.nested_scopes:
        return
    cur_scope, *next_scopes = assertion.nested_scopes
    cur_capture, *next_captures = match.captures
    name = cur_scope.capture.literal or cur_capture.node.name
    next_assertion = Query()
    next_assertion.nested_scopes = next_scopes
    inner = astNodeFromAssertion(next_assertion, SelectionMatch(next_captures))
    if 'class' in cur_scope.properties and cur_scope.properties['class']:
        return ast.ClassDef(
            name=name,
            bases=[],
            keywords=[],
            body=[ast.Pass() if inner is None else inner],
            decorator_list=[]
        )
    if 'func' in cur_scope.properties and cur_scope.properties['func']:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return (
            ast.AsyncFunctionDef if cur_scope.properties.get('async')
            else ast.FunctionDef)(
            name=name,
            args=ast.arguments(
                args=[],
                vararg=None,
                kwonlyargs=[],
                kw_default=[],
                kwarg=None,
                defaults=[]
            ),
            body=[ast.Pass() if inner is None else inner],
            decorator_list=[],
            returns=None
        )
    raise Exception("Could not determine a node type from the name properties")


def dictKeysAndValues(d): return d.keys(), d.values()


def select(root: PathEquatableAst, selector: Query) -> List[SelectionMatch]:
    selected: List[SelectionMatch] = []

    # TODO: dont root search at global scope, that's not the original design
    #       I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
    def search(node: PathEquatableAst, scopes, nesting_op: str or None = None, captures: Optional[List[PathEquatableAst]] = None):
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

def nextOrNone(itr: Iterator):
    try:
        return next(itr)
    except StopIteration:
        return None


# maybe rename override, since it's really eclipsing/overriding
def mergeAsts(a: PathEquatableAst, b: PathEquatableAst) -> PathEquatableAst:
    """ merge b into a, mutating a """
    if type(a) != type(b):
        return b

    assert a._fields == b._fields, "to merge ast nodes must have same attrs"

    for attr, a_val, b_val in ((f, getattr(a, f), getattr(b, f)) for f in a._fields):
        if isinstance(a_val, ast.AST):
            assert isinstance(b_val, ast.AST)
            setattr(a, attr, mergeAsts(a_val, b_val))
        if isinstance(a_val, list):
            assert isinstance(b_val, list)
            a_index = {n.path: n for n in a_val}
            b_index = {n.path: n for n in b_val}
            merged_dict = {**a_index, **b_index}
            for node_path in a_index.keys() & b_index.keys():
                merged_dict[node_path] = mergeAsts(
                    a_index[node_path], b_index[node_path])
            merged_list = [*merged_dict.values()]
            setattr(a, attr, merged_list)

    return a


def find(func, itr: Iterator):
    """
    TODO: move to general utilities
    """
    try:
        return next(filter(func, itr))
    except:
        pass


def destroy_selection(py_ast: PathEquatableAst, matches: Iterator[SelectionMatch] = {}) -> PathEquatableAst:
    """ remove the selected nodes from the AST, for destructive queries """

    class DestroySelection(ast.NodeTransformer):
        def visit(self, node: PathEquatableAst):
            target = find(lambda m: m.captures[-1].node == node, matches)
            # TODO: create a module tree from scratch for the assertion and merge the trees
            if target is not None:
                return None
            else:
                return super().visit(node)

    class FixEmptyBodies(ast.NodeTransformer):
        def visit(self, node: PathEquatableAst):
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


def assert_(py_ast: PathEquatableAst, assertion: Query, matches: Optional[Iterator[SelectionMatch]]) -> PathEquatableAst:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen
    """
    if matches is None:
        matches = set()

    class Transformer(ast.NodeTransformer):
        def visit(self, node):
            target = find(lambda m: m.captures[0].node == node, matches)
            # TODO: create a module tree from scratch for the assertion and merge the trees
            if target is not None:
                transformed_node = astNodeFromAssertion(assertion, target)
                # FIXME: needs to work with argument scopes as well...
                return mergeAsts(node, transformed_node)
            else:
                return super().visit(node)

    transformed_tree = Transformer().visit(py_ast)
    fixed_tree = ast.fix_missing_locations(transformed_tree)

    return fixed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = parseAst(src)
    print('pre-dump:')
    print(ast.dump(py_ast, annotate_fields=True, include_attributes=True))
    print("#########################################")
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
    print('post-dump:')
    print(ast.dump(py_ast))
    print("#########################################")
    result = astor.to_source(py_ast)
    print(result)
    return result
