#lang sicp
(#%require sicp-pict)

(define (make-segment a b)
  (cons a b))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
