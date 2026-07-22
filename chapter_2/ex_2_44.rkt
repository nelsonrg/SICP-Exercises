#lang sicp
(#%require sicp-pict)

(define (up-split-old painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-old painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split-old einstein 1))
