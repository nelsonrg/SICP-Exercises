#lang sicp

(define (n-positive x)
  (apply + (map
            (lambda (y)
              (if (> y 0)
                  1
                  0))
            x)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define   (mul-interval2 x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((= 4 (n-positive (list x1 x2 y1 y2))) (make-interval (* x1 y1) (* x2 y2)))    ; ++,++
          ((= 0 (n-positive (list x1 x2 y1 y2))) (make-interval (* x2 y2) (* x1 y1)))    ; --,--
          ((and (= 0 (n-positive (list x1)))
                (= 3 (n-positive (list x2 y1 y2)))) (make-interval (* x1 y2) (* x2 y2))) ; -+,++
          ((and (= 0 (n-positive (list y1)))
                (= 3 (n-positive (list x1 x2 y2)))) (make-interval (* y1 x2) (* y2 x2))) ; ++,-+
          ((and (= 0 (n-positive (list x1 x2)))
                (= 2 (n-positive (list y1 y2)))) (make-interval (* x1 y2) (* x2 y1))) ; --,++
          ((and (= 0 (n-positive (list y1 y2)))
                (= 2 (n-positive (list x1 x2)))) (make-interval (* y1 x2) (* y2 x1))) ; ++,--
          ((and (= 1 (n-positive (list x1 x2)))
                (= 0 (n-positive (list y1 y2)))) (make-interval (* x2 y1) (* x1 y1))) ; -+,--
          ((and (= 1 (n-positive (list y1 y2)))
                (= 0 (n-positive (list x1 x2)))) (make-interval (* y2 x1) (* y1 x1))) ; --,-+
          ((and (= 1 (n-positive (list x1 x2)))
                (= 1 (n-positive (list y1 y2))))
           ;; -+,-+
           (if (< x1 y1)
               (make-interval (* x1 y2) (* x2 y2))
               (make-interval (* y1 x2) (* x2 y2))))
          )))

(define   (make-condition name x y)
  (list name x y))

(define   (condition-name condition)
  (car condition))

(define   (condition-intervals condition)
  (cdr condition))

(define   (test-multiply condition)
  (newline) (display (condition-name condition))
  (newline) (display "mul-interval: ") (display
                                        (apply mul-interval (condition-intervals condition)))
  (newline) (display "mul-interval2: ") (display
                                         (apply mul-interval2 (condition-intervals condition))))

(define   conditions
  (list
   (make-condition "++ , ++"
                   (make-interval 1 2)
                   (make-interval 11 12))
   (make-condition "-- , --"
                   (make-interval -1 -2)
                   (make-interval -11 -12))
   (make-condition "-+ , ++"
                   (make-interval -1 2)
                   (make-interval 11 12))
   (make-condition "++ , -+"
                   (make-interval 1 2)
                   (make-interval -11 12))
   (make-condition "-- , ++"
                   (make-interval -2 -1)
                   (make-interval 11 12))
   (make-condition "++ , --"
                   (make-interval 1 2)
                   (make-interval -12 -11))
   (make-condition "-+ , --"
                   (make-interval -1 2)
                   (make-interval -12 -11))
   (make-condition "-- , -+"
                   (make-interval -2 -1)
                   (make-interval -12 11))
   (make-condition "-+ , -+"
                   (make-interval -1 2)
                   (make-interval -12 11))))

(for-each test-multiply conditions)
