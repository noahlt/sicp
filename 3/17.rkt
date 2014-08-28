#lang r5rs

(define (count-pairs structure)
  (define already-seen (list))
  (define (f x)
    (if (or (not (pair? x))
            (member x already-seen))
        0
        (begin (set! already-seen (cons x already-seen))
               (+ (f (car x))
                  (f (cdr x))
                  1))))
  (f structure))

(define (bad-count-pairs x)
  (if (not (pair? x))
      0
      (+ (bad-count-pairs (car x))
         (bad-count-pairs (cdr x))
         1)))

(define (test expected-count input-struct)
  (display "expected: ")
  (display expected-count)
  (display ", our procedure yields ")
  (display (count-pairs input-struct))
  (display ", ben's procedure yields ")
  (display (bad-count-pairs input-struct))
  (display "\n"))

(test 3 (list 1 2 3))
(test 3 (list 1 2 1))

(define bad4 (list 1 2 3))
(set-car! (cdr bad4) (cddr bad4))
(test 3 bad4)

(define bad5 (list 1 2 3))
(set-car! bad5 (cdr bad5))
(test 3 bad5)

(define bad7 (list 1 2 3))
(set-car! (cdr bad7) (cddr bad7))
(set-car! bad7 (cdr bad7))
(test 3 bad7)

(define bad-cycle (list 1 2 3))
(set-cdr! (cddr bad-cycle) bad-cycle)
(display "expected: 3, our procedure yields ")
(display (count-pairs bad-cycle))
(display ", ben's procedure never terminates.")