(require "uiop")

(defun make-morph-analyzer ()
  (labels (
           (pairs-to-lexkeys (list-of-pairs)
             (mapcar
               #'(lambda (pair)
                   (let ((pos (cadr pair))
                         (phon (car pair)))
                     (make-lexkey :pos (second pair) :phon (first pair))))
               list-of-pairs))) 

    (cond ((probe-file (*state* :fst-path))
            (let ((proc (uiop:launch-program
                          (list "flookup" "-b" "-x" (concatenate 'string
                                                                 (pathname-name (*state* :fst-path))
                                                                 "."
                                                                 (pathname-type (*state* :fst-path))))
                          :input :stream :output :stream)))
              #'(lambda (word)
                  (write-line word (uiop:process-info-input proc))
                  (force-output (uiop:process-info-input proc))
                  (labels 
                    ((wrap-string-in-parentheses (str)
                       (concatenate 'string "(" str ")")))
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
                          (funcall table (car item) (cdr item))
                          table)
                      (aux:read-from-file (*state* :mrf-path))
                      :initial-value (aux:multiset-table))))
              #'(lambda (word) ; word is string
                  (mapcar 
                    #'pairs-to-lexkeys 
                    (let ((input (intern (string-upcase word))))
                      (handler-case (funcall mrf-table input)
                        (SIMPLE-ERROR (err)
                                      (mapcar
                                        #'(lambda (lexkey)
                                            (list (list (lexkey-phon lexkey) (lexkey-pos lexkey))))
                                        (remove-if-not
                                          #'(lambda (lexkey)
                                              (equal (lexkey-phon lexkey) input))
                                          (funcall (*state* :lexicon) :keys))))))))))
          (t
            (error
              (make-condition 'missing-morph-file
                              :file-name (pathname-name (*state* :fst-path))))))))
