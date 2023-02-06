;; Structure and Interpretation of Computer Programs - page 47.

#lang racket

; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; f(n) = 2n
(define (f n) (A 0 n))

; g(0) = A(1, 0) = 0
; g(1) = A(1, 1) = 2
; g(2) = A(1, 2) = A(0, A(1, 1)) = A(1, 1) * 2 = 4
; g(3) = A(1, 3) = A(0, A(1, 2)) = A(1, 2) * 2 = 8
; g(n) = if n = 0 then 0, else 2^n
(define (g n) (A 1 n))

; h(0) = A(2, 0) = 0
; h(1) = A(2, 1) = 2
; h(2) = A(2, 2) = A(1, A(2, 1)) = A(0, A(1, A(2, 1) - 1)) = 2 * A(1, A(2, 1) - 1) = 2 * A(1, 1) = 4
; h(3) = A(2, 3) = A(1, A(2, 2)) = A(1, 4) = 16
; h(4) = A(2, 4) = A(1, A(2, 3)) = A(1, 16) = 65536
; h(5) = A(2, 5) = A(1, A(2, 4)) = A(1, 65536) = ...
; 0, 2^1, 2^2, 2^4, 2^16, 2^65536
;      0,   1,   2,    4,      16
; h(n) = if n = 0 then 0, else if n = 1 = 2, else h(n - 1)^2
(define (h n) (A 2 n))

(println (h 2))
(println (h 3))
(println (h 4))
