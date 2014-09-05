#lang r5rs

;; impl of error from http://stackoverflow.com/questions/3120379/drracket-r5rs-and-the-error-procedure
;;; create binding for error
(define error #f)

;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display ">>>> ERROR ")
                  (newline)
                  (k error-arguments)))
              'done))

;; doubly-linked list

(define (dll-cons x next prev)
  (cons x (cons next prev)))

(define (lonely-dll x)
  (dll-cons x '() '()))

(define (dll-item triple)
  (car triple))

(define (dll-next triple)
  (cadr triple))

(define (dll-prev triple)
  (cddr triple))

(define (set-dll-item! triple new-value)
  (set-car! triple new-value))

(define (set-dll-next! triple new-next)
  (set-car! (cdr triple) new-next))

(define (set-dll-prev! triple new-prev)
  (set-cdr! (cdr triple) new-prev))

(define (print-dll triple dir)
  (define (print-items triple)
    (display (dll-item triple))
    (if (not (null? (dir triple)))
        (begin (display " ")
               (print-items (dir triple)))
        '()))
  (display "(")
  (if (null? triple)
      '()
      (print-items triple))
  (display ")"))

(define (print-dll-forward triple)
  (print-dll triple dll-next))

(define (print-dll-reverse triple)
  (print-dll triple dll-prev))

;; deque

(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque x) (set-car! deque x))
(define (set-rear-ptr! deque x) (set-cdr! deque x))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called on empty deque")
      (dll-item (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called on empty deque")
      (dll-item (rear-ptr deque))))

(define (front-insert-deque! deque x)
  (cond ((empty-deque? deque)
         (set-front-ptr! deque (lonely-dll x))
         (set-rear-ptr! deque (front-ptr deque))
         deque)
        (else
         (let ((new-triple (dll-cons x (front-ptr deque) '())))
           (set-dll-prev! (front-ptr deque) new-triple)
           (set-front-ptr! deque new-triple)
           deque))))

(define (rear-insert-deque! deque x)
  (cond ((empty-deque? deque)
         (set-rear-ptr! deque (lonely-dll x))
         (set-front-ptr! deque (rear-ptr deque))
         deque)
        (else
         (let ((new-triple (dll-cons x '() (rear-ptr deque))))
           (set-dll-next! (rear-ptr deque) new-triple)
           (set-rear-ptr! deque new-triple)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE called on empty deque"))
        (else
         (set-front-ptr! deque (dll-next (front-ptr deque)))
         (set-dll-prev! (front-ptr deque) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE called on empty deque"))
        (else
         (set-rear-ptr! deque (dll-prev (rear-ptr deque)))
         (set-dll-next! (rear-ptr deque) '())
         deque)))

(define (print-deque deque)
  (display "forward: ")
  (print-dll-forward (front-ptr deque))
  (display "\nreverse: ")
  (print-dll-reverse (rear-ptr deque))
  (display "\n\n"))

; Don't know what the book's warning about printing circular lists was about.

;; testing code
(display "> (make-deque)\n")
(define dq (make-deque))
(print-deque dq)

(display "empty deque? ")
(display (empty-deque? dq))
(display "\n\n")

(display "> (rear-insert-deque! dq 'a)\n")
(rear-insert-deque! dq 'a)
(print-deque dq)

(display "empty deque? ")
(display (empty-deque? dq))
(display "\n\n")

(display "> (rear-insert-deque! dq 'b)\n")
(rear-insert-deque! dq 'b)
(print-deque dq)

(display "> (front-insert-deque! dq 'c)\n")
(front-insert-deque! dq 'c)
(print-deque dq)

(display "> (front-delete-deque! dq)\n")
(front-delete-deque! dq)
(print-deque dq)

(display "> (rear-delete-deque! dq)\n")
(rear-delete-deque! dq)
(print-deque dq)