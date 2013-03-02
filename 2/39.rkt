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

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

; This is so dumb. In the correct (racket's) implementation of
; foldl, the definition is:
;
;  (define (reverse-l sequence)
;    (foldl cons nil sequence))
;
(define (reverse-l sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(reverse-r '(1 2 3))
(reverse-l '(1 2 3))