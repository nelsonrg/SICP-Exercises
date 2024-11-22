#lang sicp


(define (cube x) (* x x x))

(define (range start stop inc)
  (if (> start stop)
      nil
      (cons start (range (+ start inc) stop inc))))

(define (simpson-rule f a b n)
  (let* ((h (/ (- b a) n))
         (k (range 0 n 1))
         (coef-k (map (lambda (k) (cond ((= k 0) 1)
                                        ((= k n) 1)
                                        ((odd? k) 4)
                                        (else 2)))
                        k))
         (yk (map (lambda (k) (f (+ a (* k h)))) k)))
    (* (/ h 3) (apply + (map * coef-k yk)))))

(simpson-rule cube 0 1 100)
(simpson-rule cube 0 1 1000)
