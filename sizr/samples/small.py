class C:
    x = 2
    def f(self): return 5

    def G(self):
        return self.f() + C.x


y = C.f
