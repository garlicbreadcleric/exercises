;; Structure and Interpretation of Computer Programs - page 61.

#lang racket

(require rackunit)
(require racket/trace)

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (not (even? x)))

(define (fast-mul x y)
  (define (fast-mul-iter s x y)
    (cond ((= y 0) s)
          ((even? y) (fast-mul-iter s (double x) (halve y)))
          (else (fast-mul-iter (+ s x) x (- y 1)))))
  (trace fast-mul-iter)
  
  (fast-mul-iter 0 x y))

(trace fast-mul)


;; Tests.

(check-equal? (fast-mul 2 50) 100)
(check-equal? (fast-mul 3 25) 75)
