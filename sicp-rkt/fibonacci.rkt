;; Structure and Interpretation of Computer Programs - page 50.

#lang racket

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fib-iter 1 0 0 n))))

(define (fib-iter fib-n-1 fib-n-2 i n)
  (if (= i n)
    fib-n-2
    (fib-iter (+ fib-n-1 fib-n-2) fib-n-1 (+ i 1) n)))

(printf "fib(3) = ~s\n" (fib 3))
(printf "fib-rec(3) = ~s\n" (fib-rec 3))

(printf "fib(5) = ~s\n" (fib 5))
(printf "fib-rec(5) = ~s\n" (fib-rec 5))

(printf "fib(7) = ~s\n" (fib 7))
(printf "fib-rec(7) = ~s\n" (fib-rec 7))

(printf "fib(10) = ~s\n" (fib 10))
(printf "fib-rec(10) = ~s\n" (fib-rec 10))
