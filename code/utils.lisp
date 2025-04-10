;;;
;;; SmallWorld
;;; 
;;; utilities
;;;


(defmacro with-debug (expression &key (message "DEBUG:") (transform #'identity))
  `(let ((val ,expression))
     (when (*state* :verbose)
       (format t "~A~%~{~A~%~}~%" ,message (funcall ,transform val) ))
     val))

(defun pretty-print (&key form type format)
  (let ((*logical-constants* '(cond and or equal))
        (*operators* '(lam forall exists))) 
    (labels 
      ((parse (form)
         "first pass parser
          sexp --> string"
         (labels 
           ((logical-constant-p (form)
              (member form *logical-constants*))

            (operator-p (form)
              (member form *operators*))

            (transform-atom (form)
              (cond ((= 1 (aux:symbol-length form)) form)
                    ((aux:gensym-p form)
                     (concatenate 'string
                                  "x_"
                                  (string (aux:symbol-char form (- (aux:symbol-length form) 1)))))
                    ((string= (symbol-name form) "NEG") (symbol-name form))
                    (t (concatenate 'string
                                    (symbol-name form)
                                    "'")))))

           (cond ((null form) nil)

                 ((atom form) (format nil "~(~A~)" (transform-atom form)))

                 ((operator-p (first form))
                   (format nil "~(~A~) ~(~A~) . ~A"
                           (first form)
                           (transform-atom (second form))
                           (parse (third form))))

                 ((logical-constant-p (first form))
                   (format nil "~A ~(~A~)"
                           (parse (second form))
                           (first form)))
                 (t
                  (let ((parse-of-rest (parse (second form))))
                    (format nil
                            (if (find #\Space parse-of-rest :test #'equalp)
                                "~A (~A)"
                                "~A ~A")
                            (parse (first form))
                            parse-of-rest))))))   

       (finisher (text)
         (let ((patterns '(("\\( " "(")
                           (" \\)" ")")
                           ("^\\(" " ")
                           ("\\)$" " ")
                           )))
           (do ((current text (re:regex-replace-all (caar pats) current (cadar pats)))
                (pats patterns (cdr pats)))
               ((null pats) current))))
       
       (variable-p (form)
         (and (symbolp form) (char= (char (symbol-name form) 0) #\?)))
       
       (sem-to-text (form)
         "lf to text printer"
         (finisher (apply
           #'concatenate
           'string
           (let ((table `(("forall" . ,(string #\U2200))
                          ("exists" . ,(string #\U2203))
                          ("and"    . ,(concatenate 'string " " (string #\U2227) " "))
                          ("neg"    . ,(concatenate 'string " " (string #\U223C) " "))
                          ("or"     . ,(concatenate 'string " " (string #\U2228) " "))
                          ("cond"   . ,(concatenate 'string " " (string #\U2283) " "))
                          ("equal"  . ,(concatenate 'string " " (string #\=) " "))
                          ("lam"    . ,(string #\U1D6CC)))
                        ))
             (mapcar
               #'(lambda (x)
                   (let ((match (assoc x table :test #'string-equal)))
                     (cond (match (rest match))
                           ((or (string= x "'") (string= x ".")) x)
                           ((= 1 (length x)) (concatenate 'string x " "))
                           ((and (= 3 (length x)) (char= (aref x 1) #\_))
                            (concatenate 'string x ""))
                           (t x))))
               (remove-if
                         #'(lambda (x) (string= x " "))
                         (sb-unicode:words (parse form))))))))

       (sem-to-tex (form)
                   "lf to tex printer"
                   (apply
                     #'concatenate
                     'string
                     (let ((table '(("forall" . "\\forall")
                                    ("exists" . "\\exists")
                                    ("and"    . "\\land")
                                    ("neg"    . "\\neg")
                                    ("or"     . "\\lor")
                                    ("cond"   . "\\rightarrow")
                                    ("equal"   . "=")
                                    ("lam"    . "\\lambda"))
                                  ))
                       (mapcar
                         #'(lambda (x)
                             (let ((match (assoc x table :test #'string-equal)))
                               (cond (match (rest match))
                                     ((or (string= x "'") (string= x ".")) x)
                                     (t x))))
                         (remove-if
                                   #'(lambda (x) (string= x "A"))
                                   (sb-unicode:words (parse form)))))))

       (syn-to-text (form)
                    (labels ((functor-p (form)
                               (and (= (length form) 3) (assoc 'in form)))
                             (atomic-p (form)
                               (consp form))
                             (print-slash (slash)
                               (let ((dir (second (assoc 'dir slash)))
                                     (mode (second (assoc 'mode slash))))
                                 (format nil "~A~A"
                                         (case dir
                                           (forward #\/)
                                           (backward #\\))
                                         (case mode
                                           (star #\*)
                                           (dot "")))))
                             (print-features (feature-list)
                               (if (endp feature-list)
                                   ""
                                   (format nil "[~{~a~^,~}]" (mapcar #'cadr feature-list))))
                             (find-bundle (form)
                               (let ((bundle (find-if
                                                     #'(lambda (x)
                                                         (every 
                                                           #'variable-p 
                                                           (mapcar
                                                             #'cadr
                                                             (set-difference (cdr x) form :test #'equalp))))
                                                     (*state* :category-bundles))))

                                 (format nil "~A~A"
                                         (car bundle)
                                         (print-features (remove-if
                                                                   #'(lambda (x)
                                                                       (variable-p (cadr x)))
                                                                   (set-difference
                                                                     form
                                                                     (cdr bundle)))))))

                             (print-atom (form)
                               (format nil "~A" (find-bundle form))))

                      ;BODY
                      (cond ((functor-p form)
                              (format nil "(~A~A~A)"
                                      (syn-to-text (second (assoc 'out form)))
                                      (print-slash (second (assoc 'slash form)))
                                      (syn-to-text (second (assoc 'in form)))))
                            ((atomic-p form) (print-atom form))
                            )))

       (sign-to-text (sign)
                     (string-downcase (format nil "[~A = ~A : ~A]"
                                              (sign-phon sign)
                                              (finisher (syn-to-text (sign-syn sign)))
                                              (sem-to-text (sign-sem sign))))))
      
        (funcall 
            (case type 
              (:sem (case format
                      (:tex #'sem-to-tex) 
                      (:text #'sem-to-text)))
              (:syn (case format
                      (:tex #'identity) 
                      (:text #'syn-to-text)))
              (:sign (case format
                      (:tex #'identity) 
                      (:text #'sign-to-text)))
              )
            form))))
