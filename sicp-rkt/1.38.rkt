;; Structure and Interpretation of Computer Programs - page 94.

#lang racket

(define (frac-iter n d k)
  (define (frac- n d k r)
    (cond ((= k 0) r)
          (else (frac- n d (- k 1) (/ (n k) (+ (d k) r))))))
  (frac- n d k 0))

(define e
  (+ 2
     (frac-iter (const 1.0)
                (lambda (i) (cond ((= (remainder (+ i 1) 3) 0) (* (/ (+ i 1) 3) 2))
                                  (else 1)))
                10)))

(printf "e = ~s\n" e)
