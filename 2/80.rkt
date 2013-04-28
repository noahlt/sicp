#lang racket

;; totally untested

(define (=zero? x y)
  (apply-generic '=zero? x y))

; inside scheme-number-package
(put '=zero? '(scheme-number)
     (lambda (x) (= 0 x)))

; inside rational-package
(put '=zero? '(rational)
     (lambda (x) (= (numer x) 0)))

; inside complex-package
(put '=zero? '(complex)
     (lambda (z)
       (and (= 0 (real-part z))
            (= 0 (imag-part z)))))
