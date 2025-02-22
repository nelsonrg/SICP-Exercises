#lang sicp


(define (n-tan x i)
  (if (= i 1)
      x
      (* x x -1)))

(define (d-tan i)
  (- (* 2 i) 1))

(define (cont-frac n d k)
  (define (cont-frac-i i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cont-frac-i (+ i 1))))))
  (cont-frac-i 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (n-tan x i))
             d-tan
             k))

(define test-values '(0.0001 3.141592 2 10))
(map (lambda (x) (exact->inexact (tan-cf x 20))) test-values)
(map tan test-values)

