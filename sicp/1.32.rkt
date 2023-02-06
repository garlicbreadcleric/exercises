;; Structure and Interpretation of Computer Programs - page 81.

#lang racket

(require rackunit)

(define (accumulate op def term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (op result (term a)))))
  (iter a def))

(define (product term a next b) (accumulate * 1 term a next b))
(define (sum term a next b) (accumulate + 0 term a next b))

(define (inc x) (+ x 1))

(define (id x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (* x 2))

(define (factorial n) (product id 1 inc n))

(check-equal? (factorial 0) 1)
(check-equal? (factorial 2) 2)
(check-equal? (factorial 3) 6)
(check-equal? (factorial 4) 24)
(check-equal? (factorial 5) 120)
