#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (elt)
                         (if (pair? elt)
                             (count-leaves elt)
                             1))
                       t)))

(count-leaves '((1 2) ((3 4) ((5 6) 7))))

