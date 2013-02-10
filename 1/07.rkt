(define (sqrt-iter guess x)
  (display guess)
  (display "\n")
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt-iter2 guess x)
  (display guess)
  (display "\n")
  ;(display (improve guess x))
  ;(display "\n")
  (if (< (abs (- guess (improve guess x)))
         (/ (improve guess x) 1000))
      guess
      (sqrt-iter2 (improve guess x) x)))

(define (my-sqrt x)
  (sqrt-iter 1 (exact->inexact x)))

(define (my-sqrt2 x)
  (sqrt-iter2 1 (exact->inexact x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; Utilities
(define (average x y)
  (/ (+ x y) 2))

(define (difference a b)
  (abs (- a b)))

(define (square x)
  (* x x))

(define (compare x)
  (display (my-sqrt x))
  (display "\n")
  ;(display (square (my-sqrt x)))
  ;(display "\n")
  (display (my-sqrt2 x)))
  ;(display "\n")
  ;(display (square (my-sqrt2 x))))

; For large numbers, checking the absolute differerence between (square guess)
; and x is sufficient, but this fails on square roots smaller than 1 because a
; difference of 0.001 is actually a 1% error.  One way to get around this, as
; suggested, is to continue iterating until the successive improvements have 
; very small differences.  By scaling the error margin with the size of the
; number, you ensure that small numbers have small error margins, but large 
; numbers will likewise have large error margins.  So the first method is more
; accurate for large numbers.
