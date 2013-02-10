#lang racket

; O(n) solution :)
(define (reverse l)
  (define (helper orig-list reversed-list)
    (if (null? orig-list)
        reversed-list
        (helper (cdr orig-list) (cons (car orig-list) reversed-list))))
  (helper l (list)))
               

(reverse (list 1 2 3 4 5))
