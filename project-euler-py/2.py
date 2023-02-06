# https://projecteuler.net/problem=2

import time


# Naive version (~0.05ms).

def _1():
    def fib(n):
        prev = 1
        current = 1

        if n < 1:
            return current

        for i in range(1, n + 1):
            p = prev
            prev = current
            current = p + current
        
        return current

    start = time.time()

    total = 0
    i = 0
    while True:
        f = fib(i)
        if f > 4_000_000: break
        if f % 2 == 0:
            total += f
        i += 1
    
    end = time.time()

    print(f'Naive: {total}, {(end - start) * 1000:.4f}ms')
_1()


# Second version: iterate (~0.009ms).

def _2():
    def fib_sum():
        i = 0
        fib = 0
        prev = 0
        total = 0

        while True:
            if i == 0:
                fib = 1
                i += 1
            elif i == 1:
                fib = 2
                prev = 1
                i += 1
            else:
                fib_ = fib
                prev_ = prev
                prev = fib_
                fib = prev_ + fib_
                # prev = prev_ + 2 * fib_
                # fib = 2 * prev_ + 3 * fib_
                # i += 3
                i += 1
            if fib > 4_000_000:
                break
            if fib % 2 == 0:
                total += fib
        return total
    
    start = time.time()
    total = fib_sum()
    end = time.time()

    print(f'Iterative: {total}, {(end - start) * 1000:.4f}ms')
_2()


# Third version: iterate and skip unimportant elements (~0.006ms).

def _3():
    def fib_sum():
        i = 0
        fib = 0
        prev = 0
        total = 0

        while True:
            if i == 0:
                fib = 1
                i += 1
            elif i == 1:
                fib = 2
                prev = 1
                i += 1
            else:
                fib_ = fib
                prev_ = prev
                prev = prev_ + 2 * fib_
                fib = 2 * prev_ + 3 * fib_
                i += 3
            if fib > 4_000_000:
                break
            if fib % 2 == 0:
                total += fib
        return total

    start = time.time()
    total = fib_sum()
    end = time.time()
    print(f'Iterative+: {total}, {(end - start) * 1000:.4f}ms')
_3()

