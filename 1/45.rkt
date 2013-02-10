#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; 2√60 = 7.74596
; 3√60 = 3.91487
; 4√60 = 2.78315

(define (root x n)
  (let ((dampings (* 2 n)))
    (fixed-point ((repeated average-damp dampings)
                  (lambda (y) (/ x (expt y (- n 1)))))
                 2.0)))

(define (test-root-convergence n)
  (define (helper i)
    (newline)
    (if (> i n)
        (display "done")
        (and (display (root 123456789 i))
             (helper (+ i 1)))))
  (helper 2))

(test-root-convergence 15)

; I note that this method appears to rapidly decline in accuracy as you
; pass the seventh root of n.