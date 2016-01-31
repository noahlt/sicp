#lang planet neil/sicp

;; Quick definitions of put and get, which were assumed to be provided.
(define *table* '())

(define (put op type item)
  (if (null? item)
      (error "Not allowed to store null in the table.")
      (set! *table* (cons (list op type item) *table*))))

(define (get op type)
  (define (helper remaining-rows)
    (cond ((null? remaining-rows)
           '())
          ((and (equal? op (caar remaining-rows))
                (equal? type (cadar remaining-rows)))
           (caddar remaining-rows))
          (else
           (helper (cdr remaining-rows)))))
  (helper *table*))

;;;
;;; The Core of the Evaluator
;;;

; list-of-values takes as an argument the operands of the combination,
; and evaluates each operand and returns a list of the corresponding values.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (sicp-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Eval (and built in special forms)

(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let ((form (get 'forms (car exp))))
                (if (null? form)
                    (sicp-apply (sicp-eval (operator exp) env)
                           (list-of-values (operands exp) env))
                    (form exp env))))))

(put 'forms 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'forms 'lambda (lambda (exp env)
                       (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)))
(put 'forms 'begin (lambda (exp env)
                     (eval-sequence (begin-actions exp) env)))

;; Apply

(define (sicp-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (sicp-eval (first-exp exps) env))
        (else (sicp-eval (first-exp  exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Conditionals

(define (eval-if exp env)
  (if (true? (sicp-eval (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(put 'forms 'if eval-if)

;; Assignments and Definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (sicp-eval (assignment-value exp) env)
                       env)
  'ok)

(put 'forms 'set! eval-assignment)

(define (eval-definition exp env)
  (display "hello hello hello")
  (display (definition-variable exp))
  (display (definition-value exp))
  (define-variable!
    (definition-variable exp)
    (sicp-eval (definition-value exp) env)
    env)
  'ok)

(put 'forms 'define eval-definition)

;; Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Representing quotation

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; Representing assignment

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; Representing definitions

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; Representing lambda expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Representing conditionals

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; this is a mildly surprising semantic for the language to have,
               ; and it's unfortunate that it emerges as an implementation detail
               ; here, in expression representation.

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Representing begin expressions

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; Representing procedure application

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


;; Derived Expressions

(define (macro m)
  (lambda (exp env)
    (sicp-eval (m exp) env)))

; Derived expression: cond

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-cond-clauses (cond-clauses exp)))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car (clauses)))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-cond-clauses rest))))))

(put 'forms 'cond (macro cond->if))

; Derived expression: and
; (and a b) -> (if (not a) 'false b)
(define (expand-and terms)
  (if (null? (cdr terms))
      (car terms)
      (make-if (list 'not (car terms))
               'false
               (and->if (cdr terms)))))

(define (and->if exp)
  (expand-and (cdr exp)))

(put 'forms 'and (macro and->if))

; Derived expression: or
; (or a b) -> (if a a b)
(define (expand-or terms)
  (if (null? (cdr terms))
      (car terms)
      (make-if (car terms)
               (car terms)
               (or->if (cdr terms)))))

(define (or->if exp)
  (expand-or (cdr exp)))

(put 'forms 'or (macro or->if))

;;;
;;; Evaluator Data Structures
;;;

;; Representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; Representing environments

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; Operations on environments

; Returns the value that is bound to the symbol VAR in the environment ENV,
; or signals an error if the variable is unbound.
(define (lookup-variable-value var env)
  (define (env-loop env) ; I'm pretty surprised that the SICP authors shadowed this variable
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Returns a new environment, consisting of a new frame in which the symbols
; in the list VARIABLES are bound to the corresponding elements in the list
; VALUES, where the enclosing environmnet is the environment BASE-ENV.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Adds to the first frame in the environment ENV a new binding that associates
; the variable VAR with the value VALUE.
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Changes the binding of the variable VAR in the environment ENV so that the
; variable is now bound to the value VALUE, or signals an error if the variable
; is unbound.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Setup the environment.
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list ; List operations:
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        ; Arithmetic:
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        ; I/O
        (list 'display display)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;;
;;; Running the Evaluator
;;;

(define the-global-environment (setup-environment))

(define input-prompt  "sicp-eval input:")
(define output-prompt "sicp-value output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (sicp-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)