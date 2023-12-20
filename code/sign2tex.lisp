(load "aux.lisp")

(defparameter *input*
  '(FORALL X
           ((COND (WOMAN X))
            (FORALL G381 ((COND (BOOK G381)) ((READ G381) X))))))

(defun logical-constant-p (form)
  (member form '(cond and or)))

(defun operator-p (form)
  (member form '(forall exists lam)))

(defun transform-atom (form)
  (cond ((= 1 (aux:symbol-length form)) form)
        ((aux:gensym-p form)
         (concatenate 'string
                      "x_"
                      (string (aux:symbol-char form (- (aux:symbol-length form) 1)))))
        (t (concatenate 'string
                        (symbol-name form)
                        "'"))))

(defun variable-p (form)
  (or
    (aux:one-char-sym-p form)
    (and
      ()
      (let ((name  (symbol-name form)))
        (concatenate 'string "x_" (string (char name (- (length name) 1))))
        ))))

(defun parse (form)
  (cond ((null form) nil)

        ((atom form) (format nil "~(~A~)" (transform-atom form)))

        ((operator-p (first form))
         (format nil "~(~A~) ~(~A~).~A"
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
