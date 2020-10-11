"""
AST transformation engine prototype for Sizr transform language
"""

from operator import and_
import libcst as cst
from typing import Optional, List, Set, Iterable, Tuple, Sequence, Union
from functools import reduce
from .code import Query, Transform, ScopeExpr, pattern_any, CaptureExpr, Match
from .cst_util import unified_visit, node_types
from .util import tryFind, notFound, first, only, tryFirst, partialPathMatch, RelativePathDict
import operator
import difflib
from os import environ as env


def matchesScopeProps(node: cst.CSTNode, scope_expr: ScopeExpr) -> bool:
    property_testers = {
        'func': lambda val, node: bool(val) == isinstance(node, cst.FunctionDef),
        'class': lambda val, node: bool(val) == isinstance(node, cst.ClassDef),
        'var': lambda val, node: bool(val) == isinstance(node, cst.Assign)
    }
    return all(map(lambda k, v: property_testers[k](v, node),
                   scope_expr.properties.keys(),
                   scope_expr.properties.values()))


# XXX: should be cleared between selections on ASTs
_next_param_cache = {}


def getNextParam(node: Union[cst.FunctionDef, cst.Parameters, cst.Param]) -> cst.Param:
    if isinstance(node, (cst.FunctionDef, cst.Parameters)):
        params = None
        if isinstance(node, cst.FunctionDef):
            params = node.params.params
        else:  # isinstance(node, cst.Parameters):
            params = node.params
        _next_param_cache.update(
            {p for p in zip(params[::1], params[1::1])})
        if len(params) > 0:
            return (params[0],)
    else:
        try:
            next_param = _next_param_cache[node]
            return (next_param,)
        except KeyError:
            return ()


def iff_on_boolean(s: Set[cst.CSTNode]):
    """return the set or its complement depending on the boolean value"""
    def func(val):
        if val == True:
            return s
        elif val == False:
            return node_types - s
        else:
            return node_types
    return func


possible_node_classes_per_prop = {
    'func': iff_on_boolean({cst.FunctionDef}),
    'class': iff_on_boolean({cst.ClassDef}),
    'var': iff_on_boolean({cst.Assign}),
    'async': lambda _: {cst.FunctionDef}
}


nesting_op_children_getter = {
    '.': lambda node: node.body.body if isinstance(node, cst.ClassDef) else (),
    '(': lambda node: node.params.params if isinstance(node, cst.FunctionDef) else (),
    ',': getNextParam,
    None: lambda node: node.children
}


per_path_default_kwargs = RelativePathDict({
    (cst.ClassDef, cst.FunctionDef): lambda path: {
        # TODO: if node is not decorated as staticmethod or class method!
        'params': cst.Parameters(params=[cst.Param(cst.Name('self'))])
    },
})


# NOTE: it is bad design to allow users to craft coincidentally unambiguous
# scope expressions, instead, it would be better to have a plenty of unambigous
# property keys (i.e. func, class, async, static, etc) for the user to craft with
def possibleElemTypes(scope_expr: ScopeExpr) -> Set[cst.CSTNode]:
    return reduce(and_,
                  map(lambda key, val: possible_node_classes_per_prop[key](val),
                      scope_expr.properties.keys(),
                      scope_expr.properties.values()),
                  node_types)


def elemName(node: cst.CSTNode) -> Optional[str]:
    node_class = node.__class__
    per_node_class_name_accessors = {
        cst.FunctionDef: lambda: node.name.value,
        cst.ClassDef: lambda: node.name.value,
        cst.Param: lambda: node.name.value,
        cst.Assign: lambda: node.targets[0] if len(node.targets) == 1 else None,
        cst.AssignTarget: lambda: node.target,
    }
    return per_node_class_name_accessors.get(node_class, lambda: None)()

# future code organization
# default_prop_values = {}
# node_from_prop_builders = {cst.ClassDef: lambda props: cst.ClassDef }


def getNodeBody(node: cst.CSTNode):
    if not hasattr(node, 'body'):
        return (), None

    body = node.body
    BodyType = None
    while not isinstance(body, Sequence):
        BodyType = type(body)
        body = body.body if hasattr(body, 'body') else []

    return body, BodyType


def astNodeFromAssertion(transform: Transform,
                         match: Match,
                         index=0) -> Sequence[cst.CSTNode]:
    # TODO: aggregate intersect possible_nodes_per_prop and and raise on multiple
    # results (some kind of "ambiguity error"). Also need to match with anchor placement
    cur_scope_expr = transform.assertion.nested_scopes[index]
    cur_capture = match.by_name.get(cur_scope_expr.capture.name)
    name = cur_scope_expr.capture.name
    body = ()
    node = BodyType = None

    if cur_capture is not None:
        node = cur_capture.node
        name = elemName(node=node)
        body, BodyType = getNodeBody(node)

    if index < len(transform.assertion.nested_scopes) - 1:
        # inner doesn'make sense for ( and , nesting scopes...
        inner = astNodeFromAssertion(transform, match, index+1)
        if transform.destructive:
            body = [s for s in body if not s.deep_equals(
                match.path[-1].node)]
        body = (*body, *inner)

    BodyType = BodyType or cst.IndentedBlock
    if not body:
        body = [cst.SimpleStatementLine(body=[cst.Pass()])]

    if cur_capture is not None:
        return [node.with_changes(
            name=cst.Name(name),
            **({
                'body': BodyType(body=body),
                # probably need a better way to do this, ideally just ignore excess kwargs
            } if isinstance(node, (cst.FunctionDef, cst.ClassDef)) else {})
        )]
    else:
        unrefed_node = astNodeFrom(
            scope_expr=cur_scope_expr, ctx=transform, match=match)
        # NOTE: need a generic way to "place" the next scope in a node
        if index < len(transform.assertion.nested_scopes) - 1:
            return [unrefed_node.with_changes(body=BodyType(body=body))]
        else:
            return [unrefed_node]


# TODO: have this check type and dispatch to astNodeFromAssertion
def astNodeFrom(scope_expr: ScopeExpr, ctx: Transform, match: Match) -> cst.CSTNode:
    scope_elem_types = possibleElemTypes(scope_expr=scope_expr)

    scope_stack = [
        (m := match.by_name.get(s.capture.name))
        and type(m)
        or first(possibleElemTypes(scope_expr=s))
        for s in ctx.assertion.nested_scopes
    ]

    # XXX: currently the path matching only takes the first match, probably it should merge all (excluding body)
    # matches into one dict
    scope_elem_extra_kwargs = per_path_default_kwargs.get(
        scope_stack, lambda _: {})(scope_stack)

    scope_elem_type = only(scope_elem_types)  # ambiguity error if not only

    # XXX: switch to giving TransformCtx CaptureReference's get_name_for_match
    name = scope_expr.capture.pattern.pattern
    return scope_elem_type(
        **{
            **({'name': cst.Name(name), }
               if 'name' in scope_elem_type.__slots__ else {}),
            **({'body': cst.IndentedBlock(body=[cst.SimpleStatementLine(body=[cst.Pass()])]), }
               if 'body' in scope_elem_type.__slots__ else {}),
            **({
                'params': cst.Parameters(),
                'decorators': (),
                **({
                    'asynchronous': cst.Asynchronous()
                } if scope_expr.properties.get('async') else {})
            } if scope_elem_type is cst.FunctionDef else {}),
            **({
                'targets': [cst.AssignTarget(target=cst.Name(name))],
                'value': cst.Name("None")
            } if scope_elem_type is cst.Assign else {}),
            **scope_elem_extra_kwargs
        }
    )


# TODO: may be preferable to convert the selector into a bytecode of path manip instructions
def select(root: cst.Module, transform: Transform) -> Transform:
    matches: List[Match] = []

    # TODO: dont root search at global scope, that's not the original design
    # I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
    def search(node: cst.CSTNode, scopes, nesting_op: Optional[str] = None, captures: Optional[List[cst.CSTNode]] = None):
        if captures is None:
            captures = []
        # XXX: need to think out this algorithm slightly more (base cases are lacking)
        if not scopes:
            return
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_children_getter[nesting_op](node):
            # FIXME: autopep8 is making this really ugly... (or maybe I am)
            name = elemName(node=node)
            if ((cur_scope.capture.pattern == pattern_any
                 # TODO: switch to elemName(node=*)
                 or (name and cur_scope.capture.pattern.match(name) is not None))
                    and matchesScopeProps(node, cur_scope)):
                next_captures = [*captures,
                                 cur_scope.capture.contextualize(node=node)]
                if rest_scopes:
                    search(node, rest_scopes,
                           cur_scope.nesting_op, next_captures)
                else:
                    matches.append(Match(next_captures))

    search(root, transform.selector.nested_scopes)

    # maybe should have search return the result list
    return transform.contextualize(root, matches)


def assert_(py_ast: cst.CSTNode, transform: Transform) -> cst.CSTNode:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen. Maybe *ensure*?
    """
    matches = transform.matches

    first_ref_index = None
    find_attempt = tryFirst(transform.capture_reference_indices)
    if find_attempt is not notFound:
        _, (first_ref_index, _) = first(transform.capture_reference_indices)

    @ unified_visit
    class Transformer(cst.CSTTransformer):
        def _leave(self, original: cst.CSTNode, updated: cst.CSTNode) -> cst.CSTNode:
            # TODO: if global scope query create a module tree from scratch?
            # NOTE: in the future can cache lib cst node comparisons for speed
            match = notFound
            if first_ref_index is not None:
                match = tryFind(lambda m: original.deep_equals(
                    m.path[first_ref_index].node), matches)
            if match is not notFound:
                from_assert = first(astNodeFromAssertion(transform, match))
                return from_assert
            elif original in transform.references:
                # TODO: replace references to anything destroyed by the transform
                pass
            elif find_attempt is notFound and isinstance(updated, cst.Module):
                module_match = Match(
                    [CaptureExpr().contextualize(node=updated)])
                return updated.with_changes(body=(*updated.body, *astNodeFromAssertion(transform, module_match)))
            else:
                return updated

    transformed_tree = py_ast.visit(Transformer())

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = cst.parse_module(src)
    old_ast = py_ast
    if env.get('SIZR_DEBUG'):
        print('<<<<< ORIGINAL:', py_ast)
    transform_ctx = None

    if transform.selector:
        transform_ctx = select(py_ast, transform)
        if env.get('SIZR_DEBUG'):
            print('#> Matches <#########################')
            for m in transform_ctx.matches:
                print(m)
            print('#####################################')
    if transform.assertion:
        py_ast = assert_(py_ast, transform_ctx)

    if env.get('SIZR_DEBUG') and transform.assertion:
        print('>>>>> TRANSFORMED:', py_ast)
        print('!!!!! DIFF', ''.join(
            difflib.unified_diff(
                str(old_ast).splitlines(1),
                str(py_ast).splitlines(1)
            )
        ))
    result = py_ast.code
    diff = ''.join(
        difflib.unified_diff(
            src.splitlines(1),
            result.splitlines(1)
        )
    )
    if diff:
        print(diff)
    else:
        print('no changes!')
    return result
