#lang racket

(define (equal? a b)
  (if (or (null? a) (null? b))
      (eq? a b)
      (and (eq? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

(equal? '(a b c) '(a b c)) ; t
(equal? '(a b z) '(a b c)) ; f
(equal? '(a b c) '(a b))   ; f