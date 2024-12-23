(require "uiop")

(defun make-morph-analyzer ()
    (cond ((probe-file (*state* :fst-path))
           (let ((proc (uiop:launch-program (list "flookup" "-b" "-x" (concatenate 'string (pathname-name (*state* :fst-path)) "." (pathname-type (*state* :fst-path))))
                                          :input :stream :output :stream)))
             #'(lambda (word)
                 (write-line word (uiop:process-info-input proc))
                 (force-output (uiop:process-info-input proc))
                 (labels 
                   ((wrap-string-in-parentheses (str)
                      (concatenate 'string "(" str ")"))
                    (pairs-to-lexkeys (list-of-pairs)
                      (mapcar
                        #'(lambda (pair)
                            (make-lexkey :cat (second pair) :phon (first pair)))
                        list-of-pairs)))

                   (mapcar
                     #'(lambda (x)
                         (pairs-to-lexkeys
                           (aux:partition
                             (read-from-string
                               (wrap-string-in-parentheses x))
                             2)))
                     (do*
                       ((line
                          (read-line (uiop:process-info-output proc))
                          (read-line (uiop:process-info-output proc)))
                        (analyses (list line) (cons line analyses)))
                       ((string= line "") (rest analyses))))))))

          ((probe-file (*state* :mrf-path))
           (let ((mrf-table
                 (reduce
                   #'(lambda (table item)
                       (funcall table (print (car item)) (print (cdr item)))
                       table)
                   (aux:read-from-file (*state* :mrf-path))
                   :initial-value (aux:multiset-table))))
             #'(lambda (word) ; word is string
                 (funcall mrf-table (intern (string-upcase word))))))
          (t
           (error
             (make-condition 'missing-morph-file
                             :file-name (pathname-name (*state* :fst-path)))))))
