;; Structure and Interpretation of Computer Programs - page 53.

#lang racket

; f(n) =
;   if n < 3, then n
;   else, f(n - 1) + 2f(n - 2) + 3f(n - 3)
;
; f(3) = f(2) + 2f(1) + 3f(0) = 4
; f(4) = f(3) + 2f(2) + 3f(1) = 4 + 4 + 1 = 9
; f(5) = f(4) + 2f(3) + 3f(2) = 9 + 8 + 8 = 25


;; Recursive version.

(define (f-rec n)
  (if (< n 3) n (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

;; Iterative version.

(define (f-iter n)
  (define (f-iter- i f-n-1 f-n-2 f-n-3)
    (if (= i n)
      (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3))
      (f-iter- (+ i 1) (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3)) f-n-1 f-n-2))
    )

  (if (< n 3) n
    (f-iter- 3 2 1 0)))


;; Demo.

(printf "f-rec(3) = ~s\n" (f-rec 3))
(printf "f-iter(3) = ~s\n" (f-iter 3))

(printf "f-rec(5) = ~s\n" (f-rec 5))
(printf "f-iter(5) = ~s\n" (f-iter 5))

(printf "f-rec(10) = ~s\n" (f-rec 10))
(printf "f-iter(10) = ~s\n" (f-iter 10))

(printf "f-rec(20) = ~s\n" (f-rec 20))
(printf "f-iter(20) = ~s\n" (f-iter 20))
