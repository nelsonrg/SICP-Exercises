#lang sicp
(#%require sicp-pict)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; Exercise 2.44
(define (up-split-old painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-old painter (- n 1))))
        (below painter (beside smaller smaller)))))

(display "Exercise 2.44 - up-split")
(newline)
(paint (up-split-old einstein 1))

;; Exercise 2.45
(define (split preposition1 preposition2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split preposition1 preposition2) painter (- n 1))))
          (preposition1 painter (preposition2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(display "Exercise 2.45 - split")(newline)
(display "right-split")(newline)
(paint (right-split einstein 1))
(display "up-split")(newline)
(paint (up-split einstein 1))

;; Exercise 2.46
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

(display "2.46")(newline)
(define x (make-vect 1 2))(display "x: ")(display x)(newline)
(define y (make-vect 1 1))(display "y: ")(display y)(newline)
(display "add-vect x y: ")(display (add-vect x y))(newline)
(display "sub-vect x y: ")(display (sub-vect x y))(newline)
(display "scale-vect 10 x: ")(display (scale-vect 10 x))(newline)

;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (origin-frame2 frame)
  (car frame))
(define (edge1-frame2 frame)
  (cadr frame))
(define (edge2-frame2 frame)
  (cddr frame))

;; Exercise 2.48
(define (make-segment a b)
  (cons a b))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
