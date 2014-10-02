#lang r5rs

(define false #f)
(define true #t)

(define (show x)
  (display x)
  (display "\n"))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else 'error)))
    dispatch))

(define t (make-table equal?))
(show ((t 'lookup) 'a))
((t 'insert!) 'a 1)
(show ((t 'lookup) 'a))
((t 'insert!) 'b 2)
(show ((t 'lookup) 'b))
((t 'insert!) 'a 100)
(show ((t 'lookup) 'a))