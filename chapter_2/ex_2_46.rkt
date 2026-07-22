#lang sicp
(#%require sicp-pict)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(display "Exercise 2.46")(newline)
(define x (make-vect 1 2))(display "x: ")(display x)(newline)
(define y (make-vect 1 1))(display "y: ")(display y)(newline)
(display "add-vect x y: ")(display (add-vect x y))(newline)
(display "sub-vect x y: ")(display (sub-vect x y))(newline)
(display "scale-vect 10 x: ")(display (scale-vect 10 x))(newline)
