;; Structure and Interpretation of Computer Programs - page 80.

#lang racket

(require rackunit)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc x) (+ x 1))

(define (id x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (* x 2))

(check-equal? (sum id 1 inc 5) 15)
(check-equal? (sum square 1 inc 3) 14)
(check-equal? (sum cube 1 inc 3) 36)
(check-equal? (sum double 1 inc 5) 30)
