#lang sicp
(#%require sicp-pict)

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
