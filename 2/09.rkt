#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width interval)
  (- (upper-bound interval) (lower-bound interval)))


(define a (make-interval 3 5))
(define b (make-interval 10 12))

(display "Is the width of the sum of two intervals a function only of the\n")
(display "widths being added? ")
(= (width (add-interval a b))
   (+ (width a) (width b)))

(display "Is the width of the multiplication of two intervals a function\n")
(display "only of the widths of the factors? ")
(= (width (mul-interval a b))
   (* (width a) (width b)))