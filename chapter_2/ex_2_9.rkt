#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

(define interval1 (make-interval 5.0 10.0))
(define interval2 (make-interval 0.0 5.0))

(display "Adding intervals of same width (5.0)")
(width (add-interval interval1 interval1))
(width (add-interval interval2 interval2))
(width (add-interval interval1 interval2))

(display "Multiplying intervals of same width (5.0)")
(width (mul-interval interval1 interval1))
(width (mul-interval interval2 interval2))
(width (mul-interval interval1 interval2))
