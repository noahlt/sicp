#lang racket

(define test-exp 
 '(let ((a 1)
        (b 2)
        (c 3))
    (+ a (* b c))))

; The above test expression can be rewritten like so:
(define rewritten-test-exp
  '(let ((a 1))
     (let ((b 2))
       (let ((c 3))
         (+ a (* b c))))))
; That way, c could reference b in its definition, and b
; could reference a in its definition.

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let->combination exp)
  (let ((definitions (cadr exp))
        (body (cddr exp)))
    (cons (make-lambda (map car definitions) body)
          (map cadr definitions))))

(let->combination test-exp)

(define (nest-let single-definition body)
  (let->combination (cons 'let (cons (list single-definition) body))))

(define (let*->nested-lets exp)
  (let ((definitions (reverse (cadr exp)))
        (body (cddr exp)))
    (foldl nest-let body definitions)))

(let*->nested-lets test-exp)

; In eval it is sufficient to add a clause whose action is
;  (eval (let*->nested-lets exp) env)
; because eval will find the let expressions (which are derived
; expressions) and expand them per the let? clause.
