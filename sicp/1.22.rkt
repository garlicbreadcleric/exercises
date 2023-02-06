;; Structure and Interpretation of Computer Programs - page 70.

#lang racket

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (find-divisor n x)
    (cond ((= (remainder n x) 0) x)
          ((> (square x) n) n)
          (else (find-divisor n (+ x 1)))))

  (find-divisor n 2))

(define (prime? x)
  (= (smallest-divisor x) x))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-monotonic-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-monotonic-milliseconds) start-time))
      0))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (for ([x (range start (+ end 1))]) (timed-prime-test x)))

(search-for-primes 100000 1000000)
