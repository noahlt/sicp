#lang racket

(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2 y)))
  
(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)