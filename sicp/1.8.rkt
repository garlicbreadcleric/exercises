;; Structure and Interpretation of Computer Programs - page 33.

#lang racket

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- x (* guess guess guess))) (* 0.00001 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (cubic-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cubic-root-iter (improve guess x) x)))

(define (cubic-root x)
  (if (< x 0)
    (- (cubic-root (- x)))
    (cubic-root-iter 1.0 x)))

(printf "cubic-root(2.0) = ~s\n" (cubic-root 2.0))
(printf "cubic-root(3.0) = ~s\n" (cubic-root 3.0))
(printf "cubic-root(8.0) = ~s\n" (cubic-root 8.0))
(printf "cubic-root(27.0) = ~s\n" (cubic-root 27.0))
(printf "cubic-root(-27.0) = ~s\n" (cubic-root -27.0))
(printf "cubic-root(1000.0) = ~s\n" (cubic-root 1000.0))
(printf "cubic-root(-1000.0) = ~s\n" (cubic-root -1000.0))
