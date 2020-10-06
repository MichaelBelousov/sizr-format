"""
AST transformation engine prototype for Sizr transform language
"""

from operator import and_
import libcst as cst
from typing import Optional, List, Set, Iterable, Tuple, Sequence
from functools import reduce
from .code import Query, TransformExpr, TransformContext, ScopeExpr, pattern_any, CapturedElement, CaptureExpr, Match
from .cst_util import unified_visit, node_types
from .util import tryFind, notFound, first, only, tryFirst, stackPathMatches
import operator
import difflib
from os import environ as env


property_testers = {
    'func': lambda val, node: isinstance(node, cst.FunctionDef),
    'class': lambda val, node: isinstance(node, cst.ClassDef),
}


nesting_op_children_getter = {
    '.': lambda node: node.body.body if isinstance(node, cst.ClassDef) else (),
    '(': lambda node: node.params if isinstance(node, cst.FunctionDef) else (),
    None: lambda node: node.children
}


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

per_path_default_kwargs = {
    (cst.ClassDef, cst.FunctionDef): lambda path: {
        # TODO: if node is not decorated as staticmethod!
        'params': cst.Parameters(params=[cst.Param(cst.Name('self'))])
    },
}


# NOTE: it is bad design to allow users to craft coincidentally unambiguous
# scope expressions, instead, it would be better to have a plenty of unambigous
# property keys (i.e. func, class, async, static, etc) for the user to craft with
def possibleElemTypes(scope_expr: ScopeExpr) -> Set[cst.CSTNode]:
    return reduce(and_,
                  map(lambda key, val: possible_node_classes_per_prop[key](val),
                      scope_expr.properties.keys(),
                      scope_expr.properties.values()),
                  node_types)


def elemName(node: cst.CSTNode) -> Set[str]:
    node_class = node.__class__
    per_node_class_name_accessors = {
        cst.FunctionDef: lambda: {node.name.value},
        cst.ClassDef: lambda: {node.name.value},
        cst.Assign: lambda: {t.target.value for t in node.targets},
    }
    return per_node_class_name_accessors.get(node_class, lambda: set())()

# future code organization
# default_prop_values = {}
# node_from_prop_builders = {cst.ClassDef: lambda props: cst.ClassDef }


def getNodeBody(node: cst.CSTNode):
    if not hasattr(node, 'body'):
        return None, None

    body = node.body
    BodyType = None
    while not isinstance(body, Sequence):
        BodyType = type(body)
        body = body.body if hasattr(body, 'body') else []

    return body, BodyType


def astNodeFromAssertion(transform: TransformContext,
                         match: Match,
                         index=0) -> Sequence[cst.CSTNode]:
    # TODO: aggregate intersect possible_nodes_per_prop and and raise on multiple
    # results (some kind of "ambiguity error"). Also need to match with anchor placement
    cur_scope_expr = transform.assertion.nested_scopes[index]
    cur_capture = match.by_name(cur_scope_expr.capture.name)
    name = cur_scope_expr.capture.name
    body = ()
    node = BodyType = None

    if cur_capture is not None:
        node = cur_capture.node
        name = only(elemName(node=node))
        body, BodyType = getNodeBody(node)

    if index < len(transform.assertion.nested_scopes) - 1:
        inner = astNodeFromAssertion(transform, match, index+1)
        if transform.destructive:
            body = [s for s in body if not s.deep_equals(
                match.elem_path[-1].node)]
        body = (*body, *inner)

    BodyType = BodyType or cst.IndentedBlock
    if not body:
        body = [cst.SimpleStatementLine(body=[cst.Pass()])]

    if cur_capture is not None:
        return [node.with_changes(
            name=cst.Name(name),
            body=BodyType(
                body=body
            ),
        )]

    scope_elem_types = possibleElemTypes(
        scope_expr=cur_scope_expr)

    scope_elem_extra_kwargs = {}

    scope_stack = [match.by_name(s.capture.name)
                   or tryFirst(possibleElemTypes(scope_expr=s))
                   for s in transform.assertion.nested_scopes]

    if env.get('SIZR_DEBUG'):
        print('scope_stack:', scope_stack)

    # TODO: probably ought to have a decidated object/class for this with an overriden __contains__
    # magic method for selection paths
    for test_path, get_default_kwargs in per_path_default_kwargs.items():
        if stackPathMatches(test_path, scope_stack):
            scope_elem_extra_kwargs.update(get_default_kwargs(scope_stack))

    scope_elem_type = only(scope_elem_types)  # ambiguity error if not only

    return [(node.with_changes if node else scope_elem_type)(
        **({
            'name': cst.Name(name),
            'body': BodyType(body=body),
        } if scope_elem_type in (cst.ClassDef, cst.FunctionDef) else {}),
        **({
            'params': cst.Parameters(),
            'decorators': (),
            **({
                'asynchronous': cst.Asynchronous()
            } if cur_scope_expr.properties.get('async') else {})
        } if scope_elem_type is cst.FunctionDef else {}),
        **({
            'targets': [cst.AssignTarget(target=cst.Name(name))],
            'value': cst.Name("None")
        } if scope_elem_type is cst.Assign else {}),
        **scope_elem_extra_kwargs
    )]
    raise Exception("Could not determine a node type from the name properties")


def select(root: cst.Module, transform: TransformExpr) -> TransformContext:
    result = TransformContext(transform, root)

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
            if ((cur_scope.capture.pattern == pattern_any
                 # TODO: switch to elemName(node=*)
                 or (hasattr(node, 'name')  # TODO: prefer isinstance()
                     and cur_scope.capture.pattern.match(node.name.value) is not None))
                    # TODO: abstract to literate function "matchesScopeProps"?
                    and all(map(lambda k, v: property_testers[k](v, node),
                                cur_scope.properties.keys(),
                                cur_scope.properties.values()))):
                next_captures = [*captures,
                                 CapturedElement(cur_scope.capture, node)]
                if rest_scopes:
                    search(node, rest_scopes,
                           cur_scope.nesting_op, next_captures)
                else:
                    result.add_match(next_captures)

    search(root, transform.selector.nested_scopes)
    return result  # maybe should have search return the result list


def assert_(py_ast: cst.CSTNode, transformCtx: TransformContext) -> cst.CSTNode:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen. Maybe *ensure*?
    """
    matches = transformCtx.matches

    first_ref_index = None
    find_attempt = tryFirst(transformCtx.capture_reference_indices)
    if find_attempt is not notFound:
        _, (first_ref_index, _) = first(transformCtx.capture_reference_indices)

    @unified_visit
    class Transformer(cst.CSTTransformer):
        def _leave(self, original: cst.CSTNode, updated: cst.CSTNode) -> cst.CSTNode:
            # TODO: if global scope query create a module tree from scratch?
            # NOTE: in the future can cache lib cst node comparisons for speed
            match = notFound
            if first_ref_index is not None:
                match = tryFind(lambda m: original.deep_equals(
                    m.elem_path[first_ref_index].node), matches)
            if match is not notFound:
                from_assert = first(astNodeFromAssertion(transformCtx, match))
                return from_assert
            elif original in transformCtx.references:
                # TODO: replace references to anything destroyed by the transform
                pass
            elif find_attempt is notFound and isinstance(updated, cst.Module):
                module_match = Match([CapturedElement(CaptureExpr(), updated)])
                return updated.with_changes(body=(*updated.body, *astNodeFromAssertion(transformCtx, module_match)))
            else:
                return updated

    transformed_tree = py_ast.visit(Transformer())

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: TransformExpr) -> str:
    py_ast = cst.parse_module(src)
    old_ast = py_ast
    if env.get('SIZR_DEBUG'):
        print('<<<<< ORIGINAL:', py_ast)
    transform_ctx = None

    if transform.selector:
        transform_ctx = select(py_ast, transform)
    if transform.assertion:
        py_ast = assert_(py_ast, transform_ctx)

    if env.get('SIZR_DEBUG'):
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
