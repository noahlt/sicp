#lang racket

;; Leaf procedures

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; Tree procedures

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch  tree) (car  tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Decoding

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "[choose-branch] bad bit" bit))))

;; Encoding

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else
         (error "[encode-symbol] tree did not contain provided symbol" symbol))))

(define (contains? x set)
  (and (not (null? set))
       (or (equal? x (car set))
           (contains? x (cdr set)))))

;; Constructing leaves from frequency pairs

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  ;(display x)
  ;(newline)
  ;(display (and (not (null? set)) (car set)))
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Exercise 2.69
; trees starts as a set of leaves. As we successively merge, trees becomes a
; set of trees, until finally it is a single tree which we return.
(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (successive-merge (adjoin-set (make-code-tree (car trees) (cadr trees))
                                    (cddr trees)))))
  
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
  

;; Test Exercise 2.69

(define tree (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))
(define message (encode '(A B A C A D A B A) tree))

(display message)
(newline)
(display (decode message tree))