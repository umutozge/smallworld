;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Term Unification         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :unifier
  (:use :common-lisp)
  (:export :unify
           :tree-match
           :refresh-vars))

(in-package unifier) 

;;; The following comment applies to the use of the unifier in SmallWorld.
;;;
;;; Categories that get combined/unified never share a variable.  
;;; We ensure this by renaming all variables before inserting 
;;; items in the on-line *lexicon*


;;; Auxiliaries


(defun get-vars (fs &optional accu)
  "return the list of variables in feature structure fs"
  (if (consp fs) 
      (get-vars (cdr fs) (get-vars (car fs) accu))
      (if (variablep fs) 
          (adjoin fs accu)
          accu)))

(defun refresh-vars (fs)
  (sublis
    (mapcar
      #'(lambda (x) (cons x (gensym "?")))
      (get-vars fs))
    fs))

(defun variablep (sym)
  (and (symbolp sym) (eql (char (symbol-name sym) 0) #\?)))

(defun get-binding (x bindings)
  (let ((binding (assoc x bindings)))
    (if binding
        (or (get-binding (cdr binding) bindings)
            (cdr binding)))))

(defun set-binding (x y bindings)
  "set the binding of x to y and return the updated bindings;
   if y is a variable with a binding, set x to that binding instead of y"
  (cond ((not (variablep x)) (error "Attempt to set binding for a non-variable"))
        ((assoc x bindings)  (error (format nil "~A is already bound" x)))
        ((variablep y) (let ((b (get-binding y bindings)))
                         (if b
                             (set-binding x b bindings)
                             (push (cons x y) bindings))))
        (t (push (cons x y) bindings))))


;;; Term unification

(defun tree-match (x y &optional bindings)
	"tree pattern matcher -- adapted from Graham (1996), p. 249"
	(cond 
		((eql x y) (values bindings t))
		((assoc x bindings) (tree-match (get-binding x bindings) y bindings))
		((assoc y bindings) (tree-match x (get-binding y bindings) bindings))
		((variablep x) (values (set-binding x (sublis bindings y) bindings) t))
		((variablep y) (values (set-binding y (sublis bindings x) bindings) t))
		(t
	 (when (and (consp x) (consp y))
		 (multiple-value-bind (b2 yes) (tree-match (car x) (car y) bindings)
			 (if yes
				(tree-match (cdr x) (cdr y) b2)
				(values nil nil)))))))

(defun unify (x y)
  "term unification"
  (multiple-value-bind (bindings match) (tree-match x y)
    (when match
        (sublis bindings x))))
