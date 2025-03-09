#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero? interval)
  (and (< (lower-bound interval) 0)
       (> (upper-bound interval) 0)))

(define (div-interval x y)
  (if (or (spans-zero? x)
          (spans-zero? y))
      (error "Error, one of the intervals spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define interval1 (make-interval 5.0 10.0))
(define interval2 (make-interval -1.0 5.0))
(div-interval interval1 interval2)
