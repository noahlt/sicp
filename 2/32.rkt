#lang racket

(define (powerset s)
  (if (null? s)
      (list (list))
      (let ((rest (powerset (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset))
                          rest)))))

(powerset (list 1 2 3))

; Powerset works by finding all the subsets without the first element of
; the set, then adding each of those subsets with the first element of the
; set thrown in as well. Very much like cc from ch1.