#lang racket

(require (planet soegaard/sicp:2:1/sicp))

; Note that the transform-painter which Soegaard provides is slightly
; different from the one in the book.

(define (my-flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.1))
   painter))

(paint (my-flip-horiz einstein))

(define (rotate-180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))

(paint (rotate-180 einstein))

(define (rotate-270 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

(paint (rotate-270 einstein))