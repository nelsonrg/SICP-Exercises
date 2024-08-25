#lang sicp

(define (abs x)
  (if (>= x 0)
      x
      (- x)))

(define (cube-root guess x)
  (define tolerance 0.001)
  (define (good-enough? guess x)
    (< (abs (- (expt guess 3)
               x))
       tolerance))
  (define (improve guess x)
    (/ (+ (/ x (expt guess 2))
          (* 2 guess))
       3))
  (if (good-enough? guess x)
      guess
      (cube-root (improve guess x) x)))
