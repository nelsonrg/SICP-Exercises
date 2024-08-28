#lang sicp


;;; recursive implementation of f
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))


;;; iterative implementation of f
(define (f-iter n)
  (define (f-iter-h f_n_1 f_n_2 f_n_3 count)
    (if (= count 0)
        f_n_3
        (f-iter-h (+ f_n_1 (* 2 f_n_2) (* 3 f_n_3)) f_n_1 f_n_2 (- count 1))))
  (f-iter-h 2 1 0 n))
