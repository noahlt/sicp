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
  (define (authenticate)
    true)
  (define (dispatch m password-input)
    (cond ((not (eq? password password-input))
           (lambda (x)
             (display "wrong password")
             false))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'authenticate) authenticate)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint existing-account existing-password new-password)
  (define (dispatch m password-input)
    (if (eq? new-password password-input)
        (existing-account m existing-password)
        (lambda (x)
          (display "wrong password")
          false)))
  (if (existing-account 'authenticate existing-password)
      dispatch
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