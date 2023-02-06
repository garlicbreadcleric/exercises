;; Structure and Interpretation of Computer Programs - page 28.

#lang racket

; Newton's method
;
; Guess     Quotient              Average
; 1         (2/1) = 2             1.5
; 1.5       (2/1.5) = 1.3333      1.4167
; 1.4167    (2/1.4167) = 1.4118   1.4142
; ...

(define (average x y) (/ (+ x y) 2))

(define (abs x) (if (< x 0) (- x) x))

(define (improve guess x)
  (let ((quotient (/ x guess)))
    (average guess quotient)))

(define (good-enough? guess x)
  (< (abs (- x (* guess guess))) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))


;; Examples.

(printf "sqrt(2.0) = ~s\n" (sqrt 2.0))
(printf "sqrt(3.0) = ~s\n" (sqrt 3.0))
(printf "sqrt(4.0) = ~s\n" (sqrt 4.0))
(printf "sqrt(9.0) = ~s\n" (sqrt 9.0))
(printf "sqrt(0.01) = ~s\n" (sqrt 0.01))

; The result is very off because our chosen
; precision (0.001) is very big compared to the actual square root.
(printf "sqrt(0.000001) = ~s\n" (sqrt 0.000001))

; This never computes because the float multiplication (lack of) precision doesn't allow us to get #t
; on (good-enough? guess x)
(printf "sqrt(9^80) = ~s\n" (sqrt 21847450052839212624230656502990235142567050104912751880812823948662932355201.0))
; (println (good-enough? 147808829414345923316083210206383297601.0 21847450052839212624230656502990235142567050104912751880812823948662932355201.0))
