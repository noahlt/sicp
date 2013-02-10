#lang racket

; This works because 2 and 3 are both prime.

(define (divides? a b)
  (= 0 (remainder a b)))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car composite)
  (if (divides? composite 2)
      (+ 1 (car (/ composite 2)))
      0))

(define (cdr composite)
  (if (divides? composite 3)
      (+ 1 (cdr (/ composite 3)))
      0))

(define (print-pair p)
  (display "(")
  (display (car p))
  (display ", ")
  (display (cdr p))
  (display ")\n"))

(print-pair (cons 5 10))
(print-pair (cons 8 6))