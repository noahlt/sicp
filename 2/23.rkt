#lang racket

(define (for-each f l)
  (if (null? l)
      true
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(define (print-elts l)
  (for-each (lambda (x)
              (display x))
            l))

(print-elts (list 1 2 3 4 5))