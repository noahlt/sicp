(define (lookup search-key tree-of-records)
  (if (null? tree-of-records)
      #f
      (let ((current-key (key (entry tree-of-records))))
        (cond ((= given-key current-key) (entry tree-of-records))
              ((< given-key current-key) (left-branch tree-of-records))
              ((> given-key current-key) (right-branch tree-of-records))))))

        
