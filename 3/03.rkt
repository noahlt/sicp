#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (< balance amount)
        "insufficient funds"
        (begin (set! balance (- balance amount))
                     balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m password-input)
    (cond ((not (eq? password password-input))
           (lambda (x) "wrong password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'withdraw 'wrong-password) 50)

((acc 'withdraw 'secret-password) 50)

((acc 'deposit 'wrong-password) 14)

((acc 'deposit 'secret-password) 14)