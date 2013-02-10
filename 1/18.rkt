#lang racket

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (fast-mult a b)
  (fast-mult-helper 0 a b))

(define (fast-mult-helper a b c)
  (cond ((= b 1)
         (+ a c))
        ((odd? b)
         (fast-mult-helper (+ a c) (- b 1) c))
        ((even? b)
         (fast-mult-helper a (halve b) (double c)))))

