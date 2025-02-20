#lang sicp


(define (filtered-accumulate filterp combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filterp a)
                    (term a)
                    null-value)
                (filtered-accumulate filterp combiner null-value term (next a) next b))))


(define (square x) (* x x))

;;; From exercise 1.21
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (find-divisor n 2))

;;; From the book
(define (prime? n)
  (= n (smallest-divisor n)))


(define (inc x) (+ x 1))

(define (identity x) x)

(define (relative-prime? i n)
  (= (gcd i n) 1))

;;; sum of squares of prime numbers in interval a to b
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;;; product of all positive integers less than n that are relatively prime to n
(define (product-relative-prime n)
  (filtered-accumulate (lambda (x) (relative-prime? x n)) * 1 identity 1 inc (- n 1)))

(product-relative-prime 10)
