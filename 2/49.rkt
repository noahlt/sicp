#lang racket

(require (planet soegaard/sicp:2:1/sicp))

(define tl (make-vect 0 .99))
(define tr (make-vect .99 .99))
(define bl (make-vect 0 0))
(define br (make-vect .99 0))

(define outline
  (segments->painter (list (make-segment tl tr)
                           (make-segment tr br)
                           (make-segment br bl)
                           (make-segment bl tl))))

(define x-painter
  (segments->painter (list (make-segment tl br)
                           (make-segment bl tr))))

(define (halve x)
  (/ x 2))

(define (midpoint v1 v2)
  (make-vect (halve (+ (vector-xcor v1) (vector-xcor v2)))
             (halve (+ (vector-ycor v1) (vector-ycor v2)))))

(define tc (midpoint tl tr))
(define lc (midpoint tl bl))
(define bc (midpoint bl br))
(define rc (midpoint tr br))

(define diamond-painter
  (segments->painter (list (make-segment tc rc)
                           (make-segment rc bc)
                           (make-segment bc lc)
                           (make-segment lc tc))))

; Painted a house instead of the wave painter from the book, because
; the wave painter in the book is ugly.
(define house-painter
  (segments->painter (list (make-segment (make-vect 0.3 0.0)
                                         (make-vect 0.3 0.4))
                           (make-segment (make-vect 0.7 0.0)
                                         (make-vect 0.7 0.4))
                           (make-segment (make-vect 0.2 0.4)
                                         (make-vect 0.8 0.4))
                           (make-segment (make-vect 0.2 0.4)
                                         (make-vect 0.5 0.7))
                           (make-segment (make-vect 0.8 0.4)
                                         (make-vect 0.5 0.7))
                           (make-segment (make-vect 0.3 0.0)
                                         (make-vect 0.7 0.0)))))

(paint outline)
(paint x-painter)
(paint diamond-painter)
(paint house-painter)