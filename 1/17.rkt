#lang r5rs

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (fast-mult a b)
  (cond ((= b 1)
         a)
        ((even? b)
         (fast-mult (double a) (halve b)))
        ((odd? b)
         (+ a (fast-mult a (- b 1))))))