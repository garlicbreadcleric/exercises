;; Structure and Interpretation of Computer Programs - page 105.

#lang racket

(require racket/trace)

(define tolerance 0.01)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (fixed-point f guess)
  (trace f)
  (define (try x)
    (let ((y (f x)))
    (if (close-enough? x y)
      x
      (try y))))
  (try guess))

(define (comp f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (<= n 1) f
              (comp f
                    (repeated f (- n 1)))))

(define (average x y) (/ (+ x y) 2))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (pow x n)
  (cond ((= n 0) 1)
        (else (* x (pow x (- n 1))))))

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y))) x))

(define (root3 x)
  (fixed-point (average-dump (lambda (y) (/ x (* y y)))) x))

(define (root4 x)
  (fixed-point ((comp average-dump average-dump) (lambda (y) (/ x (* y y y)))) x))

(define (root5 x)
  (fixed-point ((comp average-dump average-dump) (lambda (y) (/ x (* y y y y)))) x))

(define (root6 x)
  (fixed-point ((comp average-dump average-dump) (lambda (y) (/ x (* y y y y y)))) x))

(define (root7 x)
  (fixed-point ((comp average-dump average-dump) (lambda (y) (/ x (* y y y y y y)))) x))

(define (root8 x)
  (fixed-point ((comp average-dump average-dump) (lambda (y) (/ x (* y y y y y y y)))) x))

(define (root9 x)
  (fixed-point ((comp average-dump (comp average-dump average-dump)) (lambda (y) (/ x (* y y y y y y y y)))) x))

(define (root x n)
  (let ((dumped ((repeated average-dump (log n 2)) (lambda (y) (/ x (pow y (- n 1)))))))
    (fixed-point dumped x)))