#lang racket

;; totally untested, because I didn't copy over the entire type system.

(define (equ? x y)
  (apply-generic 'equ? x y))

; inside scheme-number-package
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

; inside rational-package
(put 'equ? '(rational rational)
     (lambda (x y) (and (= (numer x) (numer y))
                        (= (denom x) (denom y)))))

; inside complex-package
(put 'equ? '(complex complex)
     (lambda (z1 z2)
       (and (= (real-part z1) (real-part z2))
            (= (imag-part z1) (imag-part z2)))))

