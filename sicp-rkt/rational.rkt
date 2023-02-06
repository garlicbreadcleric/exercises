;; Structure and Interpretation of Computer Programs - page 114.

#lang racket

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (make-rat x y)
  (let* ((n (if (>= (* x y) 0) (abs x) (- (abs x))))
         (d (abs y))
         (z (abs (gcd n d))))
    (cons (/ n z) (/ d z))))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (eq-rat? x y)
  (= (* (numer x) (denom y)) (* (denom x) (numer y))))
