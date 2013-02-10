#lang racket

(define dx 0.00001)

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (n-smoothed f n)
  ((repeated smooth n) f))

