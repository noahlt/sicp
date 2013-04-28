; As a result of 2.62, 2.63, and 2.64, we can do these things in
; linear time:
;
;  * convert a binary tree to an ordered list
;  * take the union/intersection of two sets represented as
;    ordered lists
;  * convert an ordered list into a balanced binary tree
;
; So we can just do those three things in that order to compute the
; union/intersection of sets represented as binary trees in linear
; time.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                            (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-set-list set1 (cdr set2)))))))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set-list (tree->list set1) (tree->list set2))))

(define (union-set-tree set1 set2)
  (list->tree (union-set-list (tree->list set1) (tree->list set2))))

