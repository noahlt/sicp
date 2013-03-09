#lang racket

(require (planet soegaard/sicp:2:1/sicp))

(define tl (make-vect 0.0 1.0))
(define tr (make-vect 1.0 1.0))
(define bl (make-vect 0.0 0.0))
(define br (make-vect 1.0 0.0))

(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter bl br split-point) painter1))
          (paint-top
           ((transform-painter split-point (make-vect 1.0 0.5) tl) painter2)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(paint (below-1 einstein einstein))

(define (below-2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(paint (below-2 einstein einstein))