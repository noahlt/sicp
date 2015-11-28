#lang r5rs

; Error

(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
    	  (write arg))
    	args)
      (newline)
      (scheme-report-environment -1))  ;; we hope that this will signal an error

; Utils

(define (show . xs)
  (if (null? xs)
      (display "\n")
      (begin
          (display (car xs))
          (display " ")
          (apply show (cdr xs)))))
                   

; Environments

(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
(define (empty-environment? env) (null? env))
(define the-empty-environment '())

; Frames

(define (make-frame . bindings) (cons 'frame bindings))
(define (empty-frame? frame) (null? (cdr frame)))
(define (get-bindings frame) (cdr frame))

; Bindings

(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))
(define (set-binding-val! binding val) (set-car! (cdr binding) val))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (list var val) (get-bindings frame))))

; Exercise 4.11

(define (extend-environment bindings base-env)
  (if (= 0 (length bindings))
      base-env
      (cons (apply make-frame bindings) base-env)))

(define (lookup-variable-value var env)
  (define (scan-frame var frame bindings)
    (if (null? bindings)
        (lookup-variable-value var (enclosing-environment env))
        (if (eq? (binding-var (car bindings)) var)
            (binding-val (car bindings))
            (scan-frame var frame (cdr bindings)))))
  (if (empty-environment? env)
      (error "Unbound variable" var)
      (scan-frame var (first-frame env) (get-bindings (first-frame env)))))

(define (set-variable-value! var val env)
  (define (scan-frame var frame bindings)
    (if (null? bindings)
        (set-variable-value! var (enclosing-environment env))
        (if (eq? (binding-var (car bindings)) var)
            (set-binding-val! (car bindings) val)
            (scan-frame var frame (cdr bindings)))))
  (if (empty-environment? env)
      (error "Unbound variable" var)
      (scan-frame var (first-frame env) (get-bindings (first-frame env)))))

(define (define-variable! var val env)
  (define (scan-frame var frame bindings)
    (if (null? bindings)
        (add-binding-to-frame! var val frame)
        (if (eq? var (binding-var (car bindings)))
            (set-binding-val! (car bindings) val)
            (scan-frame var frame (cdr bindings)))))
  (scan-frame var (first-frame env) (get-bindings (first-frame env))))

; Test

(define e1 (extend-environment '((x 1) (y 2)) the-empty-environment))
(show e1)
(show (lookup-variable-value 'x e1))
(show (lookup-variable-value 'y e1))

(set-variable-value! 'x 5 e1)
(show e1)
(show (lookup-variable-value 'x e1))
(show (lookup-variable-value 'y e1))

(define-variable! 'z 0 e1)
(show e1)
(show (lookup-variable-value 'x e1))
(show (lookup-variable-value 'y e1))
(show (lookup-variable-value 'z e1))

(define e2 (extend-environment '((a 1) (x 99)) e1))
(show e2)
(show (lookup-variable-value 'a e2))
(show (lookup-variable-value 'x e2))
(show (lookup-variable-value 'x e1))