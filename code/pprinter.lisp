(defpackage pprinter
  (:use :common-lisp)
  (:export :print-tex
           :print-text))

(defparameter pprinter::*logical-constants*
  '(cond and or neg))

(defparameter pprinter::*logical-operators*
  '(lam forall exists))

(in-package pprinter)

;; TODO
(load "/Users/umut/res/github/smallworld/code/aux.lisp")

;; TODO
(defparameter *input*
  '(FORALL X
           ((COND (WOMAN X))
            (FORALL G381 ((COND (BOOK G381)) ((READ G381) X))))))

(defun logical-constant-p (form)
  (member form *logical-constants*))

(defun operator-p (form)
  (member form *logical-operators*))

(defun transform-atom (form)
  (cond ((= 1 (aux:symbol-length form)) form)
        ((aux:gensym-p form)
         (concatenate 'string
                      "x_"
                      (string (aux:symbol-char form (- (aux:symbol-length form) 1)))))
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
  (aux:translate-string-word
    (parse form)
    `(("forall" . ,(string #\U2200))
      ("exists" . ,(string #\U2203))
      ("and"    . ,(string #\U2227))
      ("or"     . ,(string #\U2228))
      ("cond"   . ,(string #\U2283))
      ("lam"    . ,(string #\U1D6CC)))
    ))
