;; Structure and Interpretation of Computer Programs - page 104.

#lang racket

(require racket/trace)

(define (comp f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (comp (repeated f (- n 1)) f)))

(define (square x) (* x x))

(printf "~s\n" ((repeated square 2) 5))
