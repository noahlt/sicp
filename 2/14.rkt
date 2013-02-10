#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-center-width center (* center percent)))

(define (percent i)
  (/ (width i) (center i)))

(define (print-percent i)
  (display (center i))
  (display " ± ")
  (display (percent i))
  (display "%\n"))

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

(define a (make-center-percent 3.5 0.05))
(define b (make-center-percent 6.8 0.10))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Lem is right:
(print-percent (par1 a b))
(print-percent (par2 a b))

; some more investigation...
(print-percent a)
(print-percent b)
(print-percent (mul-interval a a))
(print-percent (div-interval a a))
; I note that error bounds widen with multiplication & with division.
(print-percent (div-interval b b))
(print-percent (div-interval a b))
; Multiplication IS commutative, which is nice.
(print-percent (mul-interval (mul-interval a b) b))
(print-percent (mul-interval a (mul-interval b b)))