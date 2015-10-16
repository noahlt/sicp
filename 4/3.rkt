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


(define (dispatching-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let (form (get 'forms (car exp)))
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
(put 'forms 'cond (lambda (exp env) (sicp-eval (cond->if exp) env)))
