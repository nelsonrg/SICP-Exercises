#lang sicp
(#%require sicp-pict)

;; a
(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.45 1) (make-vect 0.4 0.85))
    (make-segment (make-vect 0.4 0.85) (make-vect 0.45 0.7))
    (make-segment (make-vect 0.45 0.7) (make-vect 0.35 0.72))
    (make-segment (make-vect 0.35 0.72) (make-vect 0.2 0.65))
    (make-segment (make-vect 0.2 0.65) (make-vect 0 0.8))
    (make-segment (make-vect 0 0.65) (make-vect 0.15 0.55))
    (make-segment (make-vect 0.15 0.55) (make-vect 0.4 0.62))
    (make-segment (make-vect 0.4 0.62) (make-vect 0.38 0.3))
    (make-segment (make-vect 0.38 0.3) (make-vect 0.3 0))
    (make-segment (make-vect 0.38 0) (make-vect 0.5 0.3))
    (make-segment (make-vect 0.5 0.3) (make-vect 0.62 0))
    (make-segment (make-vect 0.7 0) (make-vect 0.62 0.3))
    (make-segment (make-vect 0.62 0.3) (make-vect 0.6 0.6))
    (make-segment (make-vect 0.6 0.6) (make-vect 1 0.3))
    (make-segment (make-vect 1 0.35) (make-vect 0.65 0.72))
    (make-segment (make-vect 0.65 0.72) (make-vect 0.55 0.7))
    (make-segment (make-vect 0.55 0.7) (make-vect 0.6 0.85))
    (make-segment (make-vect 0.6 0.85) (make-vect 0.55 1))
    (make-segment (make-vect 0.47 0.9) (make-vect 0.47 0.91))
    (make-segment (make-vect 0.53 0.9) (make-vect 0.53 0.91))
    (make-segment (make-vect 0.45 0.8) (make-vect 0.47 0.78))
    (make-segment (make-vect 0.47 0.78) (make-vect 0.5 0.77))
    (make-segment (make-vect 0.5 0.77) (make-vect 0.53 0.78))
    (make-segment (make-vect 0.53 0.78) (make-vect 0.55 0.8)))))

(paint wave)

;; b
(define (split preposition1 preposition2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split preposition1 preposition2) painter (- n 1))))
          (preposition1 painter (preposition2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

(paint (corner-split wave 4))

;; c
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

  (paint (square-limit wave 2))

