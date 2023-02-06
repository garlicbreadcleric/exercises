;; Structure and Interpretation of Computer Programs - page 27.

#lang racket

(require rackunit)

(define (sum-of-squares x y) (+ (* x x) (* y y)))

(define (sum-of-top-two-squares x y z)
  (if (>= x z)
    (if (>= y z) (sum-of-squares x y) (sum-of-squares x z))
    (if (>= y x) (sum-of-squares z y) (sum-of-squares x z))))


;; Unit tests.

(check-equal? (sum-of-top-two-squares 3 8 6) 100)
(check-equal? (sum-of-top-two-squares 2 6 8) 100)
(check-equal? (sum-of-top-two-squares 4 2 7) 65)
(check-equal? (sum-of-top-two-squares 7 1 4) 65)
(check-equal? (sum-of-top-two-squares 8 2 1) 68)
(check-equal? (sum-of-top-two-squares 2 8 0) 68)
