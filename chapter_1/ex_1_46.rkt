#lang sicp


(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y)
     2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(sqrt 144)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (let ((next (f guess)))
      (< (abs (- guess next)) 0.00001)))
  ((iterative-improve good-enough? f) first-guess))

;;; From 1.36: should be ~4.55
(fixed-point
 (lambda (x) (average x
                      (/ (log 1000)
                         (log x))))
 2.0)

