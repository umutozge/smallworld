(defpackage pprinter
  (:use :common-lisp)
  (:export :print-tex
           :print-text))

(defparameter pprinter::*logical-constants*
  '(cond and or))

(defparameter pprinter::*operators*
  '(lam forall exists))

;; TODO
(load "/Users/umut/res/github/smallworld/code/aux.lisp")

(in-package pprinter)

(defun logical-constant-p (form)
  (member form *logical-constants*))

(defun operator-p (form)
  (member form *operators*))

(defun transform-atom (form)
  (cond ((= 1 (aux:symbol-length form)) form)
        ((aux:gensym-p form)
         (concatenate 'string
                      "x_"
                      (string (aux:symbol-char form (- (aux:symbol-length form) 1)))))
        ((string= (symbol-name form) "NEG") (print (symbol-name form)))
        (t (concatenate 'string
                        (symbol-name form)
                        "'"))))

(defun parse (form)
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
         (format nil "~A ~A"
                 (parse (first form))
                 (parse (second form))))))

(defun print-text (form)
  (apply
    #'concatenate
    'string
    (let ((table `(("forall" . ,(string #\U2200))
                   ("exists" . ,(string #\U2203))
                   ("and"    . ,(concatenate 'string " " (string #\U2227) " "))
                   ("neg"    . ,(concatenate 'string " " (string #\U223C) " "))
                   ("or"     . ,(concatenate 'string " " (string #\U2228) " "))
                   ("cond"   . ,(concatenate 'string " " (string #\U2283) " "))
                   ("lam"    . ,(string #\U1D6CC)))
                 ))
      (mapcar
        #'(lambda (x)
            (let ((match (assoc x table :test #'string-equal)))
              (cond (match (rest match))
                    ((or (string= x "'") (string= x ".")) x)
                    ((= 1 (length x)) (concatenate 'string x " "))
                    ((and (= 3 (length x)) (char= (aref x 1) #\_))
                     (concatenate 'string x " "))
                    (t x))))
        (remove-if
                  #'(lambda (x) (string= x " "))
                  (sb-unicode:words (parse form)))))))

(defun print-tex (form)
  (apply
    #'concatenate
    'string
    (let ((table '(("forall" . "\\forall")
                   ("exists" . "\\exists")
                   ("and"    . "\\land")
                   ("neg"    . "\\neg")
                   ("or"     . "\\lor")
                   ("cond"   . "\\rightarrow")
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
