#lang sicp


(define (square x) (* x x))

(define (fast-expt-iter b n)
  (define (fast-expt-iter-h a m)
    (cond ((= m n) a)
          ((<= (* 2 m) n) (fast-expt-iter-h (square a) (* 2 m)))
          (else (fast-expt-iter-h (* a b) (+ m 1)))))
  (if (= n 0) 1
      (fast-expt-iter-h b 1)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define n-test (list 0 1 2 3 9 10))
(map (lambda (n) (fast-expt 2 n)) n-test)
(map (lambda (n) (fast-expt-iter 2 n)) n-test)

(define b-test (list 0 1 10 20))
(map (lambda (b) (fast-expt b 9)) b-test)
(map (lambda (b) (fast-expt-iter b 9)) b-test)
