#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(sum
 (lambda (x) (* x x x))
 1
 (lambda (x) (+ x 1))
 10)  ; 3025

(product
 (lambda (x) (/ (+ x 2) x))
 1
 (lambda (x) (+ x 1))
 25)  ; 351

(sum-iter
 (lambda (x) (* x x x))
 1
 (lambda (x) (+ x 1))
 10)  ; 3025

(product-iter
 (lambda (x) (/ (+ x 2) x))
 1
 (lambda (x) (+ x 1))
 25)  ; 351
