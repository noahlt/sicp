#lang racket

(define (make-accumulator value)
  (lambda (addend)
    (set! value (+ value addend))
    value))

(define A (make-accumulator 5))

(A 10)

(A 10)