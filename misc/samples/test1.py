"""
test sample module
"""


class C:
    def f(self):
        x = 2

        class E:
            def __init__(self):
                self.x = 5

            @staticmethod
            def f(a):
                print(a)

    def g(self): pass
    def F(self): pass


class D:
    class C:
        def x(self, a, b):
            pass


def log_error():
    pass


def log_warning():
    pass
