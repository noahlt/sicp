#lang racket

(require (planet soegaard/sicp:2:1/sicp))

; I changed my house painter from 49 to add a door.
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
                                         (make-vect 0.7 0.0))
                           (make-segment (make-vect 0.4 0.0)
                                         (make-vect 0.4 0.2))
                           (make-segment (make-vect 0.4 0.2)
                                         (make-vect 0.5 0.2))
                           (make-segment (make-vect 0.5 0.2)
                                         (make-vect 0.5 0.0)))))

(paint house-painter)

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (flip-horiz ((split op1 op2) painter (- n 1)))))
          (op1 painter (op2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

; I changed corner-split to alternate the direction that Einstein faces.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below (flip-horiz painter) top-left)
                  (below bottom-right corner))))))

(paint-hires (corner-split einstein 3))

; I changed square-limit to hit the limit as it approaches the middle of the
; image, with the large pictures at the four corners.
(define (square-limit painter n)
  (let ((quarter (rotate180 (corner-split painter n))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint-hires (square-limit einstein 3))