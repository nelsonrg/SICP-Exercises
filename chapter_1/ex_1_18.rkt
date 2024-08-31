#lang sicp


(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (mult-fast a b step)
  (display "Step ") (display step) (newline)
  (cond ((= b 0) 0)
        ((even? b) (mult-fast (double a) (halve b) (+ step 1)))
        (else (+ a (mult-fast a (- b 1) (+ step 1))))))

(define (mult-fast-iter a b)
  (define (mult-fast-iter-h a b c step)
    (display "Step ") (display step) (newline)
    (cond ((= b 0) c)
          ((even? b) (mult-fast-iter-h (double a) (halve b) c (+ step 1)))
          (else (mult-fast-iter-h a (- b 1) (+ c a) (+ step 1)))))
  (mult-fast-iter-h a b 0 0))

(display "Recursive") (newline)
(display (mult-fast 2 10 0))
(newline)

(display "Iterative") (newline)
(display (mult-fast-iter 2 10))
