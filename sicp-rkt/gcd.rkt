;; Structure and Interpretation of Computer Programs - page 63.

#lang racket

(require rackunit)
(require racket/trace)

(define (gcd-rec a b)
  (define (find-apply l p f d)
    (if (empty? l)
        d
        (if (p (car l)) (f (car l)) (find-apply (cdr l) p f d))))

  (cond ((or (= a 1) (= b 1)) 1)
        ((< b a) (gcd-rec b a))
        (else (find-apply (range 2 (+ a 1))
                          (lambda (x) (and (= (remainder a x) 0) (= (remainder b x) 0)))
                          (lambda (x) (* x (gcd-rec (/ a x) (/ b x))))
                          1))))

(define (gcd-iter a b)
  (define (gcd-iter- a b p)
    (cond ((or (= a 1) (= b 1)) p)
          ((< b a) (gcd-iter- b a p))
          (else (for/fold ([found #f]
                           [found-value 0]
                           #:result (if found
                                        (gcd-iter- (/ a found-value) (/ b found-value) (* p found-value))
                                        p))
                          ([i (range 2 (+ a 1))] #:break found)
                  (values (and (= (remainder a i) 0) (= (remainder b i) 0)) i)))))
  (gcd-iter- a b 1))

(define (fast-gcd a b)
  (if (= b 0) a (fast-gcd b (remainder a b))))
