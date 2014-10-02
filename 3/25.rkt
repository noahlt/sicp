#lang r5rs

(define false #f)
(define true #t)

(define (show x)
  (display x)
  (display "\n"))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (f key-list subtable)
        (let ((record (assoc (car key-list) (cdr subtable))))
          (if record
              (if (null? (cdr key-list))
                  (cdr record)
                  (f (cdr key-list) record))
              false)))
      (f key-list local-table))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! key-list value)
      (define (f key-list subtable)
        (let ((record (assoc (car key-list) (cdr subtable))))
          (if record
              (if (null? (cdr key-list))
                  (set-cdr! record value)
                  (f (cdr key-list) (cdr record)))
              (if (null? (cdr key-list))
                  (set-cdr! subtable
                            (cons (cons (car key-list) value)
                                  (cdr subtable)))
                  (let ((new-subtable (cons (car key-list) '())))
                    (set-cdr! subtable
                              (cons new-subtable (cdr subtable)))
                    (f (cdr key-list) new-subtable))))
          'ok))
      (f key-list local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else 'error)))
    dispatch))

; Make sure everything still works as before
(define t (make-table))
(show ((t 'lookup) '(a)))
((t 'insert!) '(a) 1)
(show ((t 'lookup) '(a)))
((t 'insert!) '(b) 2)
(show ((t 'lookup) '(b)))
((t 'insert!) '(a) 100)
(show ((t 'lookup) '(a)))

; Now try using nested keys
(show ((t 'lookup) '(x y)))
((t 'insert!) '(x y) 7)
(show ((t 'lookup) '(x y)))
((t 'insert!) '(x a) 5)
(show ((t 'lookup) '(x a)))
(show ((t 'lookup) '(a)))