#lang sicp

(define (subtract-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define interval1 (make-interval 5.0 10.0))
(define interval2 (make-interval 0.0 5.0))
(subtract-interval interval1 interval2)
