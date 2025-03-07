#lang sicp

(define (make-pair a b)
  (* (expt 2 a) (expt 3 b)))

(define (cons x y)
  (make-pair x y))

(define (car z)
  (define (car-iter x counter)
    (if (= 0 (remainder x 2))
        (car-iter (/ x 2) (+ counter 1))
        counter))
  (car-iter z 0))

(define (cdr z)
  (define (cdr-iter x counter)
    (if (= 0 (remainder x 3))
        (cdr-iter (/ x 3) (+ counter 1))
        counter))
  (cdr-iter z 0))
