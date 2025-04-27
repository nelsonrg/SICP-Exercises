#lang sicp

(define (filter p? x)
  (cond ((null? x) '())
        ((p? (car x)) (cons (car x) (filter p? (cdr x))))
        (else (filter p? (cdr x)))))

(define (same-parity x . y)
  (if (odd? x)
      (filter odd? (append (list x) y))
      (filter even? (append (list x) y))))

(display (same-parity 1 2 3 4 5 6 7)) (newline)
(display (same-parity 2 3 4 5 6 7))
