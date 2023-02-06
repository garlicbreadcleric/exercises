;; Structure and Interpretation of Computer Programs - page 80.

#lang racket

(require rackunit)

(define (product term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* result (term a)))))
        
    (iter a 1))

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
