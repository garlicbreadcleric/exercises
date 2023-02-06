;; Structure and Interpretation of Computer Programs - page 51.

#lang racket


;; Version from the book (tree-recursive).

(define (count-change-1 amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


;; My Version (tree-recursive).

; coins: 50, 25, 10, 5, 1
; (f (x - 50) [50 25 10 5 1])) + (f (x - 25) [25 10 5 1]) + (f (x - 10) [10 5 1]) + (f (x - 5) [5 1]) + x
; (f (x - 50) 50) + (f (x - 25) 25) + (f (x - 10) 10) + (f (x - 5) 5) + x

(define (count-change-2 x)
  (define (f x d)
    (cond ((< x 0) 0)
          ((= x 0) 1)
          ((= x 1) 1)

          ; Would be better to store a list of available coins instead of repeating the if-else clause
          ; for every case.
          (else (+ (if (>= d 50) (f (- x 50) 50) 0)
                   (if (>= d 25) (f (- x 25) 25) 0)
                   (if (>= d 10) (f (- x 10) 10) 0)
                   (if (>= d  5) (f (- x  5)  5) 0)
                   (if (>= d  1) (f (- x  1)  1) 0)))))
  (f x 100))


;; Demo.

(printf "count-change-1(100) = ~s\n" (count-change-1 100))
(printf "count-change-2(100) = ~s\n" (count-change-2 100))

(printf "count-change-1(25) = ~s\n" (count-change-1 25))
(printf "count-change-2(25) = ~s\n" (count-change-2 25))

(printf "count-change-1(73) = ~s\n" (count-change-1 73))
(printf "count-change-2(73) = ~s\n" (count-change-2 73))

(printf "count-change-1(200) = ~s\n" (count-change-1 200))
(printf "count-change-2(200) = ~s\n" (count-change-2 200))
