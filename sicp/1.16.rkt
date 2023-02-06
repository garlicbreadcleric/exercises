;; Structure and Interpretation of Computer Programs - page 59.

#lang racket

(require rackunit)
(require racket/trace)

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (not (even? x)))

; (define (fast-expt b n)
;   (define (fast-expt-iter b n p i)
;     (cond ((= n i) p)
;           ((= i 0) (fast-expt-iter b n b 1))
;           ((<= (* i 2) n) (fast-expt-iter b n (* p p) (* i 2)))
;           (else (fast-expt-iter b n (* b p) (+ i 1)))))
;   (fast-expt-iter b n 1 0))

(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  
  (trace fast-expt-iter)
  
  (fast-expt-iter 1 b n))

(trace fast-expt)

;; Tests

(check-equal? (fast-expt 2 3) 8)
(check-equal? (fast-expt 3 3) 27)
(check-equal? (fast-expt 2 10) 1024)
(check-equal? (fast-expt 2 100) 1267650600228229401496703205376)
