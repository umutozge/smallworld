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

    (cond ((and
             (*state* :morphology)
             (probe-file (*state* :morphology)))
           (let ((proc (uiop:launch-program
                         (list "flookup" "-b" "-x" (concatenate 'string
                                                                (pathname-name (*state* :morphology))
                                                                "."
                                                                (pathname-type (*state* :morphology))))
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

          ((not (*state* :morphology))
           (let ((lexkeys (funcall (*state* :lexicon) :keys)))
             #'(lambda (word)
                 (let ((phon (intern (string-upcase word))))
                   (list
                     (remove-if-not 
                       #'(lambda (lexkey)
                           (equal (lexkey-phon lexkey) phon))
                       lexkeys))))))

          (t
            (error
              (make-condition 'missing-morph-file
                              :file-name (pathname-name (*state* :fst-path))))))))
