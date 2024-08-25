#lang sicp


;;; Compute the absolute value of x.
;;; This is sufficiently abstracted to be in its own procudure.
(define (abs x)
  (if (>= x 0)
      x
      (- x)))

#|
Compute the cube-root of x using Newton's method.

Note that there are multiple internal procedures that are not
abstracted enough to be taken out of the cube-root scope. These
procedures are tightly coupled to the cube-root procedure. As
mentioned in the book, we will improve the abstraction later.
|#
(define (cube-root x)
  (define tolerance 0.001)
  (define (good-enough? guess x)
    (< (abs (- (expt guess 3)
               x))
       tolerance))
  (define (improve guess x)
    (/ (+ (/ x (expt guess 2))
          (* 2 guess))
       3))
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))
  (cube-root-iter 1.1 x))
