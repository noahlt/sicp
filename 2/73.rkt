#lang racket

;; Quick definitions of put and get, which were assumed to be provided.
(define *table* '())

(define (put op type item)
  (set! *table* (cons (list op type item) *table*)))

(define (get op type)
  (define (helper remaining-rows)
    (cond ((null? remaining-rows)
           (error "op/type combination not found in table"))
          ((and (equal? op (caar remaining-rows))
                (equal? type (cadar remaining-rows)))
           (caddar remaining-rows))
          (else
           (helper (cdr remaining-rows)))))
  (helper *table*))

;; Exercise 2.73

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and  (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '^ base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; a. We can't assimilate the predicates number? and same-variable? into the
;    data-directed approach because we use the operator of an expression (like
;    + or *) as the type tag for a datum.  So we can dispatch on the type of
;    any expression which includes an operation, but no expression which is an
;    atom, like a number or a variable, since those do not include operations.
;
;    We might design an alternate system with a hierarchical type system, where
;    we use for the top level type tags such as 'number, 'variable, and
;    'operation, and operations are further specified by their specific
;    operator.

; b. Procedures for taking the derivatives of sums and products:

(put 'deriv '+ (lambda (operands var)
                 (let ((addend (car operands))
                       (augend (cadr operands)))
                   (make-sum (deriv addend var)
                             (deriv augend var)))))

(put 'deriv '* (lambda (operands var)
                 (let ((multiplier (car operands))
                       (multiplicand (cadr operands)))
                   (make-sum
                     (make-product multiplier (deriv multiplicand var))
                     (make-product (deriv multiplier var) multiplicand)))))

(deriv '(* x y) 'x)

(deriv '(+ x y) 'x)

; c. Procedure for taking the derivative of exponentiation:

(put 'deriv '^ (lambda (operands var)
                 (let ((base (car operands))
                       (exponent (cadr operands)))
                   (make-product exponent
                                 (make-product (make-exponentiation base (- exponent 1))
                                               (deriv base var))))))

(deriv '(^ x 3) 'x)

; d. We would need to change the order of the arguments to the put and calls,
;    but nothing else, which seems like a sign of good abstraction.
