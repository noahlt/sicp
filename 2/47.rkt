#lang racket

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; The selectors for origin-frame & edge1-frame are identical

(define (origin-frame fr)
  (car fr))

(define (edge1-frame fr)
  (cadr fr))

; The selectors for edge2-frame are different:

(define (edge2-frame-1 fr)
  (caddr fr))

(define (edge2-frame-2 fr)
  (cddr fr))
