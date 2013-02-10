#lang racket

(define (cc amount coin-values)
  (define (first-denomination cv) (car cv))
  (define (except-first-denomination cv) (cdr cv))
  (define (no-more? cv) (null? cv))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)
(cc 100 (list 1 5 10 25 50))

; Order of the list doesn't matter. cf that exercise where we drew out
; a diagram of how cc works. We try both branches, with and without the
; first denomination, regardless of denomination size. It's like filling
; up a glass with morbles and then water, or water and then marbles,
; stopping only when the glass overflows. Order doesn't matter.
