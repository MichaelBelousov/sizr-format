class C:
    x = 2
    def f(self): return 5

    def G(self, arg2):
        return self.f() + C.x + arg2


y = C.f
