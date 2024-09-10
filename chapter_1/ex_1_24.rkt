#lang sicp


(define (square x) (* x x))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (divides? a b) (= (remainder b a) 0))
  (find-divisor n 2))

;;; From the book
(define (prime? n)
  (= n (smallest-divisor n)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it x)
    (= (expmod x n n) x))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exponent m)
  (cond ((= exponent 0) 1)
        ((even? exponent)
         (remainder (square (expmod base (/ exponent 2) m))
                    m))
        (else (remainder (* base (expmod base (- exponent 1) m))
                         m))))

;;; Provided by the problem
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;; From exercise 1.22
(define (search-for-primes start stop)
  (cond ((> start stop) (newline))
        ((even? start) (search-for-primes (+ start 1) stop))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) stop))))

(search-for-primes 1000 1019)  ; total time ~ 201
(search-for-primes 10000 10037)  ; total time ~ 382
(search-for-primes 100000 100043)  ; total time ~ 286
(search-for-primes 1000000 1000037)  ; total time ~ 323
