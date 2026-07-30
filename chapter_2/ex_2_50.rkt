#lang sicp
(#%require sicp-pict)

;; Use the wave painter
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
    (make-segment (make-vect 0.6 0.85) (make-vect 0.55 1)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(paint (beside (flip-horiz wave) wave))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(paint (rotate-180 wave))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(paint (rotate-270 wave))
