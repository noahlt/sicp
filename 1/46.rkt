#lang racket

(define (iterative-improve good-enough? improve)
  (define (r guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (r next))))
  r)

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (within-tolerance a b tolerance)
  (< (abs (- a b)) tolerance))

(define (sqrt x)
  ((iterative-improve (lambda (prev-guess next-guess)
                        (within-tolerance (square next-guess) x 0.001))
                     (lambda (guess)
                       (average guess (/ x guess))))
   2.0))

(display "√2 =\n  ")
(sqrt 2)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (prev-guess next-guess)
                        (within-tolerance prev-guess next-guess 0.00001))
                      f)
   first-guess))

(display "\nfixed point of cos =\n  ")
(fixed-point cos 1.0)