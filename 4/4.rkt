#lang racket

;; Quick definitions of put and get, which were assumed to be provided.
(define *table* '())

(define (put op type item)
  (if (null? item)
      (error "Not allowed to store null in the table.")
      (set! *table* (cons (list op type item) *table*))))

(define (get op type)
  (define (helper remaining-rows)
    (cond ((null? remaining-rows)
           null)
          ((and (equal? op (caar remaining-rows))
                (equal? type (cadar remaining-rows)))
           (caddar remaining-rows))
          (else
           (helper (cdr remaining-rows)))))
  (helper *table*))

;; Data-directed style of eval
(define (dispatching-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let ((form (get 'forms (car exp))))
                (if (null? form)
                    (sicp-apply (sicp-eval (operator exp) env)
                           (list-of-values (operands exp) env))
                    (form exp env))))))

(put 'forms 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'forms 'set! eval-assignment)
(put 'forms 'define eval-definition)
(put 'forms 'if eval-if)
(put 'forms 'lambda (lambda (exp env)
                       (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)))
(put 'forms 'begin (lambda (exp env)
                     (eval-sequence (begin-actions exp) env)))

(define (macro m)
  (lambda (exp env)
    (sicp-eval (m exp) env)))

(put 'forms 'cond (macro cond->if))

;; exercise 4.4 -- I think derived expressions are more beautiful.

(put 'forms 'and (macro and->if))
(put 'forms 'or (macro or->if))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; (and a b) -> (if (not a) 'false b)

(define (expand-and terms)
  (if (null? (cdr terms))
      (car terms)
      (make-if (list 'not (car terms))
               'false
               (and->if (cdr terms)))))

(define (and->if exp)
  (expand-and (cdr exp)))

; (or a b) -> (if a a b)
(define (expand-or terms)
  (if (null? (cdr terms))
      (car terms)
      (make-if (car terms)
               (car terms)
               (or->if (cdr terms)))))

(define (or->if exp)
  (expand-or (cdr exp)))

