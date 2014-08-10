#lang racket

(define (make-passwordless-account balance)
  (define (withdraw amount)
    (if (< balance amount)
        "insufficient funds"
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) balance)
          (else (error "unknown request to account"))))
  dispatch)

(define (make-account balance password)
  (define acct (make-passwordless-account balance))
  (define (dispatch m password-input)
    (cond ((not (eq? password-input password))
           (lambda (x)
             (display "wrong password")
             false))
          ((eq? m 'authenticate)
           true)
          ((eq? m 'get-acct)
           acct)
          ((eq? m 'set-acct)
           (lambda (new-acct)
             (set! acct new-acct)
             (acct 'balance)))
          (else
           (acct m))))
  dispatch)

(define (make-joint existing-account existing-password new-password)
  (if (existing-account 'authenticate existing-password)
      (let ((new-account (make-account 0 new-password)))
        ((new-account 'set-acct new-password)
         (existing-account 'get-acct existing-password))
        new-account)
      "wrong password for existing account; cannot create joint account"))

(define paul-acc (make-account 100 'open-sesame))

((paul-acc 'withdraw 'wrong-password) 50)

((paul-acc 'withdraw 'open-sesame) 50)

((paul-acc 'deposit 'wrong-password) 14)

((paul-acc 'deposit 'open-sesame) 14)

(define david-acc (make-joint paul-acc 'open-sesame 'melon))

((david-acc 'withdraw 'wrong-password) 10)

((david-acc 'withdraw 'melon) 10)

((paul-acc 'deposit 'open-sesame) 50)

((david-acc 'withdraw 'melon) 10)