#lang racket

(define (same-parity . number-list)
  (define (same-parity? x)
    (= (remainder x 2) (remainder (car number-list) 2)))
  (define (helper l)
    (if (null? l)
        l
        (if (same-parity? (car l))
            (cons (car l) (helper (cdr l)))
            (helper (cdr l)))))
  (helper number-list))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

          