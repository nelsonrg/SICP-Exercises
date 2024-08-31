#lang sicp


(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (mult-fast a b step)
  (display step) (newline)
  (cond ((= b 0) 0)
        ((even? b) (mult-fast (double a) (halve b) (+ step 1)))
        (else (+ a (mult-fast a (- b 1) (+ step 1))))))

(define (mult-slow a b step)
  (display step) (newline)
  (if (= b 0) 0
      (+ a (mult-slow a (- b 1) (+ step 1)))))

(mult-fast 2 10 0)
(mult-slow 2 10 0)
