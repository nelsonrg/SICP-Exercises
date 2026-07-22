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

