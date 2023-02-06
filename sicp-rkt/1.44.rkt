;; Structure and Interpretation of Computer Programs - page 104.

#lang racket

(require racket/trace)

(define dx 2.0)

(define (average3 a b c) (/ (+ a b c) 3.0))

(define (smooth f)
  (lambda (x)
    (average3 (f x) (f (- x dx)) (f (+ x dx)))))

(define (comp f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (comp
      f
      (repeated f (- n 1)))))

(define (smooth-n f n)
  ((repeated smooth n) f))

(define (cube x) (* x x x))

(printf "cube(5) = ~s\n" (cube 5))
(printf "smooth(cube)(5) = ~s\n" ((smooth cube) 5))
(printf "smooth^1(cube)(5) = ~s\n" ((smooth-n cube 1) 5))
(printf "smooth(smooth(cube))(5) = ~s\n" (((repeated smooth 2) cube) 5))
(printf "smooth^2(cube)(5) = ~s\n" ((smooth-n cube 2) 5))
(printf "smooth^3(cube)(5) = ~s\n" ((smooth-n cube 3) 5))
(printf "smooth^4(cube)(5) = ~s\n" ((smooth-n cube 4) 5))

