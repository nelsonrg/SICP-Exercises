#lang sicp


(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define counter 0)
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (begin
        (set! counter (+ counter 1))
        (p (sine (/ angle 3.0))))))

(sine 12.15)
(display counter)
