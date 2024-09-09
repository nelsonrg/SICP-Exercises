#lang sicp


(define (square x) (* x x))

;;; From exercise 1.21
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (find-divisor n 2))

;;; From the book
(define (prime? n)
  (= n (smallest-divisor n)))

;;; Provided by the problem
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start stop)
  (cond ((> start stop) (newline))
        ((even? start) (search-for-primes (+ start 1) stop))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) stop))))

(search-for-primes 1000 1019)  ; total time ~ 1
(search-for-primes 10000 10037)  ; total time ~ 5
(search-for-primes 100000 100043)  ; total time ~ 17
(search-for-primes 1000000 1000037)  ; total time ~ 52
