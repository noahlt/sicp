#lang racket

(define (every? pred list)
  (or (null? list)
      (and (pred (car list)) (every? pred (cdr list)))))

(define (some? pred list)
  (or (null? list)
      (or (pred (car list)) (some? pred (cdr list)))))

(define (inline-operators exp)
  (if (null? (cdr exp))
      '()
      (cons (cadr exp) (inline-operators (cddr exp)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and  (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (some? (lambda (op) (eq? op '+)) (inline-operators x))))

(define (addend s) (car s))

(define (augend s)
  (cond ((> (length s) 3) (cddr s))
        ((= (length s) 3) (caddr s))
        (else (error "AUGEND -- bad expression length (<= 2)"))))

(define (product? x)
  (and (pair? x) (every? (lambda (op) (eq? op '*)) (inline-operators x))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (cond ((> (length p) 3) (cddr p))
        ((= (length p) 3) (caddr p))
        (else (error "MULTIPLICAND -- bad expression length (<= 2)"))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (d/dx exp) (deriv exp 'x))

(d/dx '(x + 3 * (x + y + 2)))