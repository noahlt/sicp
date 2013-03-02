#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 1/6
(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3))  ; (((() 1) 2) 3)

; Commutative operations will have the same value across
; fold-right and fold-left.

; FUN FACT: the racket implementation of foldl behaves differently
; compared to the above implementation of fold-left!
;
; The above implementations behave like this (this isn't how
; they're implemented, but you can pretend it is because the
; behavior is the same):
; 
; fold-right translates (1 2 3)
;                  into ((1 2) 3)
;        then evaluates (op (op 1 2) 3)
; 
; fold-left translates (1 2 3)
;                 into (1 (2 3)),
;       then evaluates (op 1 (op 2 3))
;
; The normal implementations behave like this:
;
; foldr is the same as fold-right
;
; foldl reverses the list then applies foldr to it.
;
; This discrepant behavior results in these discrepant results:

(foldr / 1 (list 1 2 3)) ; 3/2
(foldl / 1 (list 1 2 3)) ; 3/2
(foldr list '() '(1 2 3)) ; (1 (2 (3 ())))
(foldl list '() '(1 2 3)) ; (3 (2 (1 ())))

; I don't know what property of an operation will cause it to be
; the same for both foldr and foldl, but it seems pretty interesting.

