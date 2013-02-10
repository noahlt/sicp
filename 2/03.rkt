#lang racket

(define (difference x y)
  (abs (- x y)))

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (make-segment a b) (cons a b))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")\n"))

(define (rect1 left top right bottom)
  (cons (cons left right)
        (cons top bottom)))

(define (left-rect1 r) (caar r))
(define (right-rect1 r) (cadr r))
(define (top-rect1 r) (cdar r))
(define (bottom-rect1 r) (cddr r))

(define (vert-side-rect1 r) (difference (top-rect1 r) (bottom-rect1 r)))
(define (horiz-side-rect1 r) (difference (left-rect1 r) (right-rect1 r)))

(define (perimeter-rect1 r)
  (+ (* 2 (horiz-side-rect1 r))
     (* 2 (vert-side-rect1 r))))

(define (area-rect1 r)
  (* (horiz-side-rect1 r)
     (vert-side-rect1 r)))



(define (rect2 p1 p2)
  (cons topleft-point bottomright-point))

(define (p1-rect2 r) (car r))
(define (p2-rect2 r) (cdr r))

(define (vert-side-rect2 r)
  (difference (y-point (p1-rect2 r))
              (y-point (p2-rect2 r))))

(define (horiz-side-rect2 r)
  (difference (x-point (p1-rect2 r))
              (x-point (p2-rect2 r))))

(define (perimeter-rect2 r)
  (+ (* 2 (horiz-side-rect2 r))
     (* 2 (vert-side-rect2 r))))

(define (area-rect2 r)
  (* (horiz-side-rect2 r)
     (vert-side-rect2 r)))
  