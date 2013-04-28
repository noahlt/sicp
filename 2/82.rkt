#lang racket

; Storage of operations and coercions. I wrote all this intending to test
; my solutions, then didn't.

(define *operations* (list))
(define *coercions* (list))

(define (put alist key value)
  (set! alist (cons (cons key value) alist)))

(define (get alist key)
  (cond ((null? alist)              #f)
        ((equal? key (caar alist))  (cdar alist))
        (else                       (get (cdr alist) key))))

(define (get-operation op-name types)
  (get *operations* op-name types))

(define (put-operation op-name types operation)
  (put *operations* op-name types operation))

(define (get-coercion op-name types)
  (get *coercions* op-name types))

(define (put-coercion op-name types coercion)
  (put *coercions* op-name types coercion))

;; Data structure for typed objects

(define (get-type x) (car x))
(define (contents x) (cdr x))
(define (tag tag-name x) (cons tag-name x))


;; Exercise 2.82 - completely untested.

(define (all? l)
  (cond ((null? l)
         true)
        ((not (car l))
         false)
        (else
         (all? (cdr l)))))

(define (try-each-coercion args types)
  (if (null? types)
      (error "No method for these types")
      (let ((coercions (map (lambda (x)
                              (get-coercion (get-type x) (get-type (car types))))
                            args)))
        (if (all? coercions)
            (map apply coercions args)
            (try-each-coercion args (cdr types))))))
  

; This approach seems weird to me. Why design it this way? Why
; not instead look up all the types supported by this operation,
; then try to coerce each of the arguments to a type that we 
; know is supported?

(define (apply-generic op . args)
  (let ((type-tags (map get-type args)))
    (let ((proc (get-operation op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-each-coercion args type-tags)))))
