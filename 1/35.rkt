#lang racket

(define tolerance 0.00001) ; from the text
(define phi 1.61803398875) ; from google

(define (golden-ratio-transform x)
  (+ 1 (/ 1 x)))

(display "Is ϕ the fixed-point of x ⇥ 1 + 1/x? ")
(display (< (abs (- phi (golden-ratio-transform phi)))
            tolerance))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(newline)
(display "Deriving ϕ by finding the fixed point of x ⇥ 1 + 1/x...\nϕ = ")
(display (fixed-point golden-ratio-transform 1.0))