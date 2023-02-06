dx = 2

def smooth(f):
    def inner(x):
        return (f(x) + f(x + dx) + f(x - dx)) / 3
    return inner

def comp(f, g):
    return lambda x: f(g(x))

def repeated(f, n):
    if n == 1:
        return f
    return comp(repeated(f, n - 1), f)

def smooth_n(f, n):
    return repeated(smooth, n)(f)

def cube(x): return x * x * x

x = repeated(smooth, 2)(cube)(5)
y = smooth_n(cube, 2)(5)

print(x, y)
