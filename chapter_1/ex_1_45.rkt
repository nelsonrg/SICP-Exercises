#lang sicp

(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess counter)
    ;;(display guess) (display ": ") (display counter) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ counter 1)))))
  (try first-guess 0))

(define (sum x)
  (apply + x))

(define (length x)
  (if (null? x)
      0
      (+ 1 (length (cdr x)))))

(define (average x)
  (/ (sum x)
     (length x)))

(define (average-damp f)
  (lambda (x) (average (list x (f x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (nth-root x n)
  (let ((ndamp (floor (log n 2))))
    (fixed-point
     ((repeated average-damp ndamp)
      (lambda (y) (/ x (power y (- n 1)))))
     1.0)))

(for-each
 (lambda (n) (display (nth-root 10 n)) (newline))
 '(2 3 4 5 6 7 8 9 10))
