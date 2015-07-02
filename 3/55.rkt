#lang racket

; I admit this is not as pretty as some of the other solutions
; online, due to using the literal for zero.

(define (partial-sums s)
  (add-streams s
               (cons-stream 0
                            (partial-sums s))))