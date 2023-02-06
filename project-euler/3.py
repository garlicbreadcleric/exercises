# https://projecteuler.net/problem=3

import time


# Naive version (~0.24ms).

def _1():
    from math import sqrt
    def factorize(x):
        factors = []
        i = 2
        while i*i <= x:
            if x % i == 0:
                factors.append(i)
                x = int(x / i)
            else:
                i += 1
        if x != 1:
            factors.append(x)
        return factors

    start = time.time()
    result = factorize(600851475143)[-1]
    end = time.time()
    print(f'Naive: {result}, {(end - start) * 1000:.4f}ms')
_1()


# Second version: don't store factors (~0.24ms).

def _2():
    from math import sqrt

    start = time.time()
    result = 1
    i = 2
    x = 600851475143
    while i*i <= x:
        if x % i == 0:
            result = i
            x = int(x / i)
        else:
            i += 1
    if x != 1:
        result = x
    end = time.time()
    print(f'Naive+: {result}, {(end - start) * 1000:.4f}ms')
_2()
