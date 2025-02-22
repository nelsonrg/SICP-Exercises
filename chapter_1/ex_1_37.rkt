#lang sicp


(define (cont-frac n d k)
  (define (cont-frac-i i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cont-frac-i (+ i 1))))))
  (cont-frac-i 1))

(define (approx-inverse-phi k)
  (let ((approx (cont-frac (lambda (i) 1.0)
                           (lambda (i) 1.0)
                           k)))
    (display k) (display ": ") (display approx) (newline)
    (if (= 0.6180
           (/ (floor (* approx 1e4))
              1e4))
        approx
        (approx-inverse-phi (+ k 1)))))

(approx-inverse-phi 1)

(define (cont-frac-iter n d k)
  (define (cont-frac-i i result)
    (if (= i 0)
        result
        (cont-frac-i (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-i k 0))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)
