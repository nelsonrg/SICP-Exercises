#lang sicp


(define (make-pascal row col)
  (cond ((= row 0) 1)                   ; First row base case
        ((or (= col 0) (= col row)) 1)  ; First and last column base case
        (else (+ (make-pascal (- row 1) (- col 1))
                 (make-pascal (- row 1) col)))))

(define (display-pascal-row n)
  (define row-length (+ n 1))
  (define (display-element col)
    (display (make-pascal n col))
    (display " ")
    (if (= (+ col 1) row-length)
        (newline)
        (display-element (+ col 1))))
  (display-element 0))

(define (display-pascal-triangle n-rows)
  (define (display-row row-number)
    (display-pascal-row row-number)
    (if (= (+ row-number 1) n-rows)
        (newline)
        (display-row (+ row-number 1))))
  (display-row 0))
