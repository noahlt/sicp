#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define c (cons 1 2))

; c = (lambda (m) (m 1 1))

; (car c)
; (c (lambda (p q)
;      p))
; ((lambda (m) (m 1 2))
;  (lambda (p q) p))
; ((lambda (p q) p) 1 2)
; (lambda (1 2) 1)
; 1