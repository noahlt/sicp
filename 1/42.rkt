#lang racket

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (incr x)
  (+ x 1))

((compose square incr) 6)