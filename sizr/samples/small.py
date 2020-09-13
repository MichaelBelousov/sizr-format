class C:
    x = 2
    def f(self): pass
    def g(self): pass

# `C . f >>! class C` destroys the entire class C and replaces
# would be solved with proper merge algorithm actually
