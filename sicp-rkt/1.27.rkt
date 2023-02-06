;; Structure and Interpretation of Computer Programs - page 73.

#lang racket

(require rackunit)
(require racket/trace)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m)) (else
               (remainder
                (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (test-a-n a n)
    (cond ((= a 0) #t)
          ((= (expmod a n n) a) (test-a-n (- a 1) n))
          (else #f)))
  
  (test-a-n (- n 1) n))

(check-equal? (fermat-test 561) #t)
(check-equal? (fermat-test 562) #f)
(check-equal? (fermat-test 1105) #t)
(check-equal? (fermat-test 1106) #f)
(check-equal? (fermat-test 1729) #t)
(check-equal? (fermat-test 1730) #f)
(check-equal? (fermat-test 2465) #t)
(check-equal? (fermat-test 2466) #f)
(check-equal? (fermat-test 2821) #t)
(check-equal? (fermat-test 2822) #f)
(check-equal? (fermat-test 6601) #t)
(check-equal? (fermat-test 6602) #f)
