#lang sicp

(define (get-sign x)
  (if (>= x 0)
      1
      -1))

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (get-sign (* n d))))
    (cons (/ (* sign (abs n)) g)
          (/ (abs d) g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
