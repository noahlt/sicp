; Did I get this far into the book without understanding how Racket's module
; system?  Yes.  Yes I did.
;
; Anyways, here is where I'll put definitions that come from the book.

(define foo "bar")

; p365
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

; p366
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

; p367

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (sicp-eval (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (sicpeval (first-exp exps) env))
        (else (sicp-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; p368

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (sicp-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (sicp-eval (definition-value exp) env)
                    env)
  'ok)



(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

; p369

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define variable? symbol?)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; I actually rewrote this to use and rather than if.
(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))


; p370

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))