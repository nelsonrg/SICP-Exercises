#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (let ((point-list (list (start-segment segment) (end-segment segment))))
    (make-point (average (map x-point point-list))
                (average (map y-point point-list)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

(define (sum x)
  (apply + x))

(define (length x)
  (if (null? x)
      0
      (+ 1 (length (cdr x)))))

(define (average x)
  (/ (sum x)
     (length x)))

(define origin (make-point 0 0))
(define p1 (make-point 1 1))
(define segment1 (make-segment origin p1))
(print-segment segment1)
(print-point (mid-point segment1))

