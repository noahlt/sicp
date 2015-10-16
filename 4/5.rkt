#lang racket

(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (sicp-eval (cond->if exp) env))
        ((application? exp)
         (sicp-apply (sicp-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; exercise 4.5

(define (assigning-form? clause)
  (equal? '=> (cadr clause)))

; This code isn't great because the interpreter will evaluate the program's
; (cond-predicate first) twice, which may be a problem if it has side-effects.
; The alternative is to transform into a lambda/let, which also isn't great,
; because it will end up shadowing a variable name, which might conflict with
; a variable name used by the evaluated program. (That is, we don't have
; hygienic macros.)

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- CONF-IF" clauses))
            (if (assigning-form? first)
                (make-if (cond-predicate first)
                         (list (caddr first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))