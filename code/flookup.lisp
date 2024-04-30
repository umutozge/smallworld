(require "uiop")

(defun setup-morph-analyzer (name)
  (let ((proc (uiop:launch-program (list "flookup" "-b" "-x" (concatenate 'string name ".fst"))
                                          :input :stream :output :stream)))
    (setf (symbol-function 'flookup)
          #'(lambda (word)
              (write-line word (uiop:process-info-input proc))
              (force-output (uiop:process-info-input proc))
              (do*
                ((line
                   (read-line (uiop:process-info-output proc))
                   (read-line (uiop:process-info-output proc)))
                 (analyses (list line) (cons line analyses)))
                ((string= line "") (rest analyses)))))))
