#lang sicp

(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-test n)
  (define (try-it x)
    (= (expmod-checked x n n) x))
  (try-it (+ 1 (random (- n 1)))))

(define (square x) (* x x))

(define (expmod-checked base exponent m)
  (cond ((= exponent 0) 1)
        ((even? exponent)
         (let* ((x (expmod-checked base (/ exponent 2) m))
                (y (remainder (square x) m)))
           (if (and (not (or (= x 1)
                             (= x (- m 1))))
                    (= y 1))
               0
               y)))
        (else (remainder (* base (expmod-checked base (- exponent 1) m)) m))))

(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))
(define primes (list 2 5 7 11))
(define (display-prime-check x)
  (display x)
  (display " : ")
  (display (miller-rabin-prime? x 10))
  (newline)
  nil)

(define (foreach lst fun)
  (fun (car lst))
  (if (not (null? (cdr lst)))
      (foreach (cdr lst) fun)))

(display "Carmichael Numbers")
(newline)
(foreach carmichael-numbers display-prime-check)
(display "Known Primes")
(newline)
(foreach primes display-prime-check)
