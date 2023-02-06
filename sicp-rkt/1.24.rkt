;; Structure and Interpretation of Computer Programs - page 72.

#lang racket

(define (square x) (* x x))

(define (next x)
  (if (< x 3) (+ x 1) (+ x 2)))

(define (smallest-divisor n)
  (define (find-divisor n x)
    (cond ((= (remainder n x) 0) x)
          ((> (square x) n) n)
          (else (find-divisor n (next x)))))

  (find-divisor n 2))

; (define (prime? x)
;   (= (smallest-divisor x) x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m)) (else
               (remainder
                (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-monotonic-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (current-inexact-monotonic-milliseconds) start-time))
      0))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (for ([x (range start (+ end 1))]) (timed-prime-test x)))

(search-for-primes 9000000 90000000)
