;; Structure and Interpretation of Computer Programs - page 53.

#lang racket

; Pascal's triangle:
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;    ...
;
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
;    ...

(define (pascal row col)
  (cond ((< col 1) 0)
        ((> col row) 0)
        ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))


;; Demo.

(printf "\
    ~s
   ~s ~s
  ~s ~s ~s
 ~s ~s ~s ~s
~s ~s ~s ~s ~s\n"
  (pascal 1 1)
  (pascal 2 1) (pascal 2 2)
  (pascal 3 1) (pascal 3 2) (pascal 3 3)
  (pascal 4 1) (pascal 4 2) (pascal 4 3) (pascal 4 4)
  (pascal 5 1) (pascal 5 2) (pascal 5 3) (pascal 5 4) (pascal 5 5))
