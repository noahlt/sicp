#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (negative? x)
  (< 0 x))

(define (positive? x)
  (> 0 x))

(define (make-positive x)
  (if (negative? x)
      (- x)
      x))

(define (make-rat n d)
  (let ((g (make-positive (gcd n d)))
        (n (if (and (negative? d) (positive? n))
               (- n)
               n))
        (d (make-positive d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (add-rat (make-rat 1 3) (make-rat 1 3)))
(print-rat (make-rat 5 -6))
(print-rat (make-rat -3 -5))