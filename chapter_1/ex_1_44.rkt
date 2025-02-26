#lang sicp


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (sum x)
  (if (null? x)
      0
      (+ (car x) (sum (cdr x)))))

(define (length x)
  (if (null? x)
      0
      (+ 1 (length (cdr x)))))

(define (average x)
  (/ (sum x)
     (length x)))

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (average (list
                (f (- x dx))
                (f x)
                (f (+ x dx)))))))

(define (smooth-nth f n)
  ((repeated smooth n) f))
