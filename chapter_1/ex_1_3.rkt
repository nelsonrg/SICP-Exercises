#lang sicp

(define (square x) (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (max-two x y z)
  (cond ((and (>= x y) (>= y z)) (list x y))
        ((and (>= x z) (>= z y)) (list x z))
        ((and (>= y x) (>= x z)) (list y x))
        ((and (>= y z) (>= z x)) (list y z))
        ((and (>= z x) (>= x y)) (list z x))
        ((and (>= z y) (>= y x)) (list z y))))

(define (sum-larger-squares x y z)
  (apply sum-squares (max-two x y z)))
