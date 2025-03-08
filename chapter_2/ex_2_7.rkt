#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define interval (make-interval 5.0 10.0))
(lower-bound interval)
(upper-bound interval)
