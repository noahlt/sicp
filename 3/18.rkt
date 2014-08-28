#lang r5rs

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (detect-cycle lst)
  (define already-seen (list))
  (define (f x)
    (cond ((not (pair? x)) #f)
          ((member x already-seen) #t)
          (else (begin (set! already-seen (cons x already-seen))
                       (f (cdr x))))))
  (f lst))

(display (detect-cycle '()))
(display (detect-cycle '(1 2 3)))
(display (detect-cycle (make-cycle '(1 2 3))))

