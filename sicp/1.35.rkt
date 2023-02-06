;; Structure and Interpretation of Computer Programs - page 94.

#lang racket

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f guess)
  (let ((next-guess (f guess)))
    
    (cond ((< (abs (- guess next-guess)) 0.001) (average guess next-guess))
          (else (fixed-point f (average guess next-guess))))))

(printf "fixed-point(cos, 1.0) = ~s\n" (fixed-point cos 1.0))
(printf "fixed-point(sin, 1.0) = ~s\n" (fixed-point sin 1.0))

(define (sqrt x) (fixed-point (lambda (y) (/ x y)) 1.0))
(define (golden-ratio)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

(printf "sqrt(4.0) = ~s\n" (sqrt 4.0))
(printf "sqrt(5.0) = ~s\n" (sqrt 5.0))
(printf "sqrt(9.0) = ~s\n" (sqrt 9.0))
(printf "sqrt(10.0) = ~s\n" (sqrt 10.0))
(printf "golden-ratio = ~s\n" (golden-ratio))
