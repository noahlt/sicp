#lang racket

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

; (while (< x 10)
;   (set! x (+ x 1)))
;
;   expands to
;
; (let _fn ()
;   (if (< x 10)
;       (begin (set! x (+ x 1))
;              (_fn))))

(define (make-named-let name definitions body)
  (cons 'let (cons name (cons definitions body))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (last-exp? seq) (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (expand-while exp)
  (let ((test (cadr exp))
        (body (cddr exp)))
    (make-named-let
     '_fn (list)
     (list (make-if test
                    (sequence->exp (append body '((_fn))))
                    false)))))

(expand-while 
 '(while (< x 10)
    (set! x (+ x 1))))