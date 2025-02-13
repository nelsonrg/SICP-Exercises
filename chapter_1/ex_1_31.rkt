#lang sicp


(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-recursive
   (lambda (x) x)
   1
   (lambda (x) (+ 1 x))
   n))

;;; https://en.wikipedia.org/wiki/Wallis_product
(define (wallis-product n)
  (define (increment x) (+ 1 x))
  (define (wallis-product-fn x)
    (* (/ (* 2 x)
          (- (* 2 x) 1))
       (/ (* 2 x)
          (+ (* 2 x) 1))))
  (product-recursive wallis-product-fn 1 increment n))

(define (wallis-approximate-pi n)
  (* 2 (wallis-product n)))
