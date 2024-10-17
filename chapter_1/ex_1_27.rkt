#lang sicp

;;; test whether a^n is congruent to a modulo n for every a < n
(define (carmichael-number? n)
  (define (congruent? a)
    (= (expmod a n n) a))
  (define (all-congruent? a)
    (cond ((= a 1) #t)
          ((not (congruent? a)) #f)
          (else (all-congruent? (- a 1)))))
  (all-congruent? (- n 1)))

(define (square x) (* x x))

(define (expmod base exponent m)
  (cond ((= exponent 0) 1)
        ((even? exponent) (remainder (square (expmod base (/ exponent 2) m)) m))
        (else (remainder (* base (expmod base (- exponent 1) m)) m))))

(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))
(define (display-carmichael-number x)
  (display x)
  (display " : ")
  (display (carmichael-number? x))
  (newline)
  nil)

(define (foreach lst fun)
  (fun (car lst))
  (if (not (null? (cdr lst)))
      (foreach (cdr lst) fun)))

(foreach carmichael-numbers display-carmichael-number)
