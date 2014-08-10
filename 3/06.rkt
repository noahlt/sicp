#lang racket

; untested

(define (rand cmd)
  (define seed random-init)
  (cond ((eq? cmd 'generate)
         (begin (set! seed (rand-update seed))
                seed)
        ((eq? cmd 'reset)
         (lambda (new-seed)
           (set! seed new-seed)))
        (else (error "must call rand with generate or reset")))))