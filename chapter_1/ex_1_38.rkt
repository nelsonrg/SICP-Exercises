#lang sicp


(define (euler-d i)
  (if (= (modulo (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

(define (cont-frac n d k)
  (define (cont-frac-i i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cont-frac-i (+ i 1))))))
  (cont-frac-i 1))

(define (approx-e k)
  (cont-frac (lambda (i) 1.0)
             euler-d
             k))

(approx-e 10)
