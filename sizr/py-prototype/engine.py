"""
AST transformation engine prototype for Sizr transform language
"""

import ast
from code import *

node_table = {
    ast.Class: {
        'func': lambda n: True,
        'class': lambda n: True,
        'name': lambda node: node.name,
    },
    ast.Function: {
        'func': True,
    },
}

class property_testers:
    @staticmethod
    def func(val: None, node: ast.Node) -> bool:
        return node.class == 'Function'


def select(py_src: str or file, selector: Query):
    selected = []
    search_root = ast.parse(py_src)
    scopes = iter(selector.nested_scopes)
    scope = next(scopes)
    # FIXME: this may reread nodes :(, makes it terribly inefficient
    for node in ast.walk(search_root):
        if scope.capture.test(node.name)
        and all(lambda p: property_testers[p.key](p.val, node), scope.properties):
            search_root = node
            scope = next(scopes)

def assert_()
    import astor
    astor.to_source(tree)

# should be just select and assert but python makes it easy to do just Transform
def transform():
    class Transformer(ast.NodeTransformer):
        Transformer[f'visit_{Node}']
    Transformer['']
    transformed_tree = Transformer.visit(tree)
    fixed_tree = ast.fix_missing_locations(transformed_tree)