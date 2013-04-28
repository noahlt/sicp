(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; This implementation of the set operations is constant-time for adjoin-set
; and union-set, but will use more memory and take longer to run
; element-of-set? and intersection-set, since they have to iterate across
; the entire list.  This implementation might be useful if we had to write
; a lot of data as quickly as possible, when memory and read-access were
; not concerns.
