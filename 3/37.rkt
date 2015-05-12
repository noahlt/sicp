#lang racket

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv v)
  (let ((x (make-connector)))
    (constant v x)
    x))
