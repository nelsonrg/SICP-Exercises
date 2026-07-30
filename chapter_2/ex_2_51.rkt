#lang sicp
(#%require sicp-pict)

;; from exercise 2.49
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


;; from the book
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-above
           (transform-painter
            painter1
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0)))
          (paint-below
           (transform-painter
            painter2
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point)))
      (lambda (frame)
        (paint-above frame)
        (paint-below frame)))))

(paint (below1 wave wave))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate-90 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below2 painter1 painter2)
  (rotate-270
   (beside (rotate-90 painter1)
           (rotate-90 painter2))))

(paint (below2 wave wave))
