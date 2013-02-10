#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (make-segment a b) (cons a b))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((x1 (x-point (start-segment seg)))
        (y1 (y-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y2 (y-point (end-segment seg))))
    (make-point (average x1 x2) (average y1 y2))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")\n"))

(print-point (midpoint-segment (make-segment (make-point 1 2)
                                             (make-point 5 8))))
