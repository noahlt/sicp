#lang r5rs

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (detect-cycle lst)
  ; Traverse lst advancing x one pair at a time and y two pairs at a time.
  (define (f x y)
    (cond ((or (not (pair? x))
               (not (pair? y))) #f)
          ((eq? x y) #t)
          (else (f (cdr x) (cddr y)))))
  (if (or (not (pair? lst))
          (not (pair? (cdr lst))))
      #f
      (f lst (cdr lst))))

(display (detect-cycle '()))
(display (detect-cycle '(1 2 3)))
(display (detect-cycle (make-cycle '(1 2 3))))