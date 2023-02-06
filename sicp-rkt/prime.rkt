;; Structure and Interpretation of Computer Programs - page 65.

#lang racket

(require rackunit)
(require racket/trace)

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (find-divisor n x)
    (cond ((= (remainder n x) 0) x)
          ((> (square x) n) n)
          (else (find-divisor n (+ x 1)))))

  (find-divisor n 2))

(define (prime? x)
  (= (smallest-divisor x) x))

(check-equal? (smallest-divisor 5) 5)
(check-equal? (smallest-divisor 6) 2)
(check-equal? (smallest-divisor 7) 7)
(check-equal? (smallest-divisor 8) 2)
(check-equal? (smallest-divisor 9) 3)

(check-equal? (prime? 5) #t)
(check-equal? (prime? 6) #f)
(check-equal? (prime? 7) #t)
(check-equal? (prime? 8) #f)
(check-equal? (prime? 9) #f)
