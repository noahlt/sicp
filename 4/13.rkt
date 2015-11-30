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

; Environments

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Frames

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; Environment operations

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (scan-frame search-var frame base-fn found-fn)
  (define (scan vars vals)
    (cond ((null? vars)
           (base-fn))
          ((eq? search-var (car vars))
           (found-fn vals))
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))

(define (env-loop search-var found-fn env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable")
      (scan-frame search-var (first-frame env)
                  (lambda ()
                    (env-loop search-var found-fn (enclosing-environment env)))
                  found-fn)))

(define (lookup-variable-value var env)
  (env-loop var car env))

(define (set-variable-value! var val env)
  (env-loop var (lambda (vals) (set-car! vals val)) env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame var frame
                (lambda ()
                  (add-binding-to-frame! var val frame))
                (lambda (vals)
                  (set-car! vals val)))))

; This implementation does the simplest, least-surprising thing I
; could think of: matches behavior of set-variable-value!, by going
; through each environment and removing the first variable it can find.
(define (undefine-variable! var env)
  (define (env-loop env)
    (define (scan vars vals prev-vars prev-vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (null? (cdr vars))
                 (begin
                   (set-cdr! prev-vars '())
                   (set-cdr! prev-vals '()))
                 (begin
                   (set-car! vars (cadr vars))
                   (set-cdr! vars (cadr vars))
                   (set-car! vals (cadr vals))
                   (set-cdr! vals (cddr vals)))))
            (else (scan (cdr vars) (cdr vals) vars vals))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)
                '() '()))))
  (env-loop env))

; Utils

(define (show . xs)
  (if (null? xs)
      (display "\n")
      (begin
          (display (car xs))
          (display " ")
          (apply show (cdr xs)))))

; Test env operations

(define (test-env-ops)
  (define e1 (extend-environment '(x y) '(1 2) the-empty-environment))
  (show e1)
  (show (lookup-variable-value 'x e1))
  (show (lookup-variable-value 'y e1))
  
  (set-variable-value! 'x 5 e1)
  (show e1)
  (show 'x (lookup-variable-value 'x e1))
  (show 'y (lookup-variable-value 'y e1))
  
  (define-variable! 'z 0 e1)
  (show e1)
  (show 'x (lookup-variable-value 'x e1))
  (show 'y (lookup-variable-value 'y e1))
  (show 'z (lookup-variable-value 'z e1))

  (undefine-variable! 'z e1)
  (show e1)
  
  (define e2 (extend-environment '(a x) '(1 99) e1))
  (show e2)
  (show 'a '(e2) (lookup-variable-value 'a e2))
  (show 'x '(e2) (lookup-variable-value 'x e2))
  (show 'x '(e1) (lookup-variable-value 'x e1)))

(test-env-ops)

; Exercise 4.13 -- special form for undefine!

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((undefinition? exp) (eval-undefinition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (undefinition? exp)
  (tagged-list? exp 'undefine!))

(define (eval-undefinition exp env)
  (undefine-variable! (cadr exp) env))