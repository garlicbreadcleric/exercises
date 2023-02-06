;; Structure and Interpretation of Computer Programs - page 61.

#lang racket

(require rackunit)
(require racket/trace)

(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (not (even? x)))

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fib-iter 1 0 2 n))))

(define (fib-iter fib-n-1 fib-n-2 i n)
  (if (= i n)
      (+ fib-n-1 fib-n-2)
      (fib-iter (+ fib-n-1 fib-n-2) fib-n-1 (+ i 1) n)))

(define (fast-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fast-fib-iter 1 0 0 1 n))))

(define (fast-fib-iter fib-n-1 fib-n-2 p q n)
  (cond ((= n 0) fib-n-2)
        ((odd? n)
         (fast-fib-iter
          (+ (* q fib-n-2)
             (* q fib-n-1)
             (* p fib-n-1))
          (+ (* p fib-n-2)
             (* q fib-n-1))
          p
          q
          (- n 1)))
        (else
         (fast-fib-iter
          fib-n-1
          fib-n-2
          (+ (* q q) (* p p))
          (+ (* q q) (* 2 q p))
          (/ n 2)))))

; (trace fast-fib-iter)

(printf "fib(3) = ~s\n" (fib 3))
(printf "fast-fib(3) = ~s\n" (fast-fib 3))
(printf "fib-rec(3) = ~s\n" (fib-rec 3))

(printf "fib(5) = ~s\n" (fib 5))
(printf "fast-fib(5) = ~s\n" (fast-fib 5))
(printf "fib-rec(5) = ~s\n" (fib-rec 5))

(printf "fib(7) = ~s\n" (fib 7))
(printf "fast-fib(7) = ~s\n" (fast-fib 7))
(printf "fib-rec(7) = ~s\n" (fib-rec 7))

(printf "fib(17) = ~s\n" (fib 17))
(printf "fast-fib(17) = ~s\n" (fast-fib 17))
(printf "fib-rec(17) = ~s\n" (fib-rec 17))

; (printf "fib(100) = ~s\n" (fib 100))
; (printf "fast-fib(100) = ~s\n" (fast-fib 100))
; (printf "fib-rec(100) = ~s\n" (fib-rec 100))