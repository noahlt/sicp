#lang racket

(define (or-gate a b out)
  (define c (make-wire))
  (define d (make-wire))
  (define e (make-wire))
  (inverter a c)
  (inverter b d)
  (and-gate c d e)
  (inverter e out))
