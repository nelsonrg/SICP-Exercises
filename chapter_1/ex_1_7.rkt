#lang sicp

#|| For numbers smaller than 0.001, the `good-enough?` procedure would
end way too soon. For example check the following: ||#

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (abs x)
  (if (>= x 0)
      x
      (- x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y)
     2))

(sqrt-iter 1 0.0000001)

;;; The right answer is ~0.000316 but this gives 0.03 because 0.03^2 is smaller than 0.001

(sqrt-iter 1 1234)

;;; I have a hard time breaking it for larger values as long as my guess is in the ballpark.

(exact->inexact (sqrt-iter 10000000 123456789123456789))
(sqrt 123456789123456789)

(define (good-enough-improved? prev-guess guess)
  (< (/ (abs (- guess prev-guess)) guess)
     0.001))

(define (sqrt-iter-improved prev-guess guess x)
  (if (good-enough-improved? prev-guess guess)
      guess
      (sqrt-iter-improved guess (improve guess x) x)))

;;; This works much better for small numbers at least and still works for large numbers.

(exact->inexact (sqrt-iter-improved 2 1 0.0000001))

(exact->inexact (sqrt-iter-improved 20000000 10000000 123456789123456789))
