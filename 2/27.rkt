#lang racket

(define (deep-reverse l)
  (define (iter orig-list reversed-list)
    (cond ((null? orig-list)
           reversed-list)
          ((not (pair? orig-list))
           orig-list)
          (else
           (iter (cdr orig-list)
                   (cons (deep-reverse (car orig-list)) reversed-list)))))
  (iter l (list)))

(deep-reverse '(1 2 (3 4 5) 6 ((7 8) 9)))