#lang sicp


(define (f g) (g 2))

(define (square x) (* x x))

(f square)

(f f) 
