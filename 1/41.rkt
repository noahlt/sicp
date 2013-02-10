#lang racket

(define (double f)
  (lambda (x)
    (f (f x))))

(define (incr x)
  (+ x 1))

((double incr) 5) ; -> 7

(((double (double double)) incr) 5) ; -> 21