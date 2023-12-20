(set-macro-character #\!
                     #'(lambda (str chr)
                         (declare (ignore chr))
                         (list 'lam (read str t nil t))))

