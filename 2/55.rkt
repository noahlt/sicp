#lang racket

(car ''abracadabra)

; is equivalent to

(car (quote (quote abacadabra)))

; or

(car '(quote abacadabra))

; which is why the interpreter printed "quote".