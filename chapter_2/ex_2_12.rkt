#lang sicp

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-percent ctr pct)
  (let ((width (* ctr pct)))
    (make-interval (- ctr width) (+ ctr width))))

(make-center-percent 15 0.1)

(define (percent i)
  (/ (width i) (center i)))

(percent (make-center-percent 15 0.1))
