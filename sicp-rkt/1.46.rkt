;; Structure and Interpretation of Computer Programs - page 105.

#lang racket

(require racket/trace)

(define (y f) (lambda (x) (f f x)))

(define (iterative-improve good-enough? improve)
  (y (lambda (self x) (if (good-enough? x) x (self self (improve x))))))

(define tolerance 0.01)

(define (average x y) (/ (+ x y) 2))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (define (good-enough? y)
    (< (abs (- x (* y y))) tolerance))
  
  (define (f y) (/ x y))

  ((iterative-improve good-enough? (average-dump f)) x))
