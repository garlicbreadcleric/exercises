;; Structure and Interpretation of Computer Programs - page 103.

#lang racket

(require racket/trace)

(define dx 0.00001)

(define (deriv f) (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next) next (try next))))
  ; (trace try)
  (try first-guess))
; (trace fixed-point)

(define (newton-transform g)
  (let ((f (lambda (x) (- x (/ (g x) ((deriv g) x))))))
    ; (trace f)
    f))
; (trace newton-transform)

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
; (trace newtons-method)

(define (average a b) (/ (+ a b) 2))
; (trace average)

(define (average-dump f)
  (lambda (x) (average x (f x))))
; (trace average-dump)

(define (square x) (* x x))
; (trace square)
(define (cube x) (* x x x))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))
; (trace sqrt)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (solve-cubic a b c) (newtons-method (cubic a b c) 1.0))

(printf "sqrt(4.0) = ~s\n" (sqrt 4.0))
(printf "sqrt(5.0) = ~s\n" (sqrt 5.0))
(printf "sqrt(9.0) = ~s\n" (sqrt 9.0))

(printf "solve-cubic(1, 2, 3) = ~s\n" (solve-cubic 1 2 3))
