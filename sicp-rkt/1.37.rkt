;; Structure and Interpretation of Computer Programs - page 94.

#lang racket

(require racket/trace)

(define (cont-frac n d k)
  (define (frac- n d k i)
    (let ((n-i (n i))
          (d-i (d i)))
      (cond ((= i k) (/ n-i d-i))
            (else (/ n-i (+ d-i (frac- n d k (+ i 1))))))))
  (trace frac-)
  (frac- n d k 1))

(define (cont-frac-iter n d k)
  (define (frac- n d k r)
    (cond ((= k 0) r)
          (else (frac- n d (- k 1) (/ (n k) (+ (d k) r))))))
  (trace frac-)
  (frac- n d k 0))

(trace cont-frac)
(trace cont-frac-iter)

(printf "cont-frac(const(1.0) const(1.0) 20) = ~s\n"
  (cont-frac (const 1.0) (const 1.0) 20))

(printf "cont-frac-iter(const(1.0) const(1.0) 20) = ~s\n"
  (cont-frac-iter (const 1.0) (const 1.0) 20))
