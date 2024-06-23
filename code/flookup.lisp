(require "uiop")

(defun setup-morph-analyzer (name)
  
  (let* ((fst-file (concatenate 'string name ".fst"))
        (proc (uiop:launch-program (list "flookup" "-b" "-x" fst-file)
                                          :input :stream :output :stream)))
    (if (probe-file fst-file)
        (setf (symbol-function 'flookup)
              #'(lambda (word)
                  (write-line word (uiop:process-info-input proc))
                  (force-output (uiop:process-info-input proc))
                  (do*
                    ((line
                       (read-line (uiop:process-info-output proc))
                       (read-line (uiop:process-info-output proc)))
                     (analyses (list line) (cons line analyses)))
                    ((string= line "") (rest analyses)))))   
        (error
          (make-condition 'missing-fst
                          :file-name fst-file)))))
