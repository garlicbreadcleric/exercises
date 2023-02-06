# https://projecteuler.net/problem=1

import time


# Naive version (~0.12ms).

def _1():
    start = time.time()
    total = 0
    for i in range(1, 1000):
        if i % 3 == 0 or i % 5 == 0:
            total += i
    end = time.time()

    print(f'Naive: {total}, {(end - start) * 1000:.4f}ms')
_1()


# Second version: group by 15 (~0.017ms).

def _2():
    start = time.time()
    total = 0
    i = 0
    x = 3 + 5 + 6 + 9 + 10 + 12
    while i < 1000:
        if i + 15 < 1000:
            total += i * 7 + x
            i += 15
        else:
            if i % 3 == 0 or i % 5 == 0:
                total += i
            i += 1
    end = time.time()

    print(f'Iterative: {total}, {(end - start) * 1000:.4f}ms')
_2()
