
(load "setutils.lisp")

(defpackage :models
  (:use :common-lisp)
  (:export :interpret  
           :generate-model
           :display-model
           :refresh-model
		   ))

(in-package models) 

(setf *random-state* (make-random-state t))

(defparameter *generate-entity* (let ((count 0 ))
                                  #'(lambda () (incf count))) "a closure for the stock of domain entities")

(defparameter *entity-domain* (mapcar 
                                 #'(lambda (x) (or x (funcall *generate-entity*)))
                                 (make-list 5 :initial-element nil)))

(defparameter *relation-vocabulary* '((0 john mary bill sue)
									  (1 happy biologist pianist dog cat chinese human sleeps walks)
									  (2 love hate chase)
									  ))

(defparameter *manual-vocabulary* '((be #'identity)
									(a #'identity)
									(not #'(lambda (f)
											 #'(lambda (x)
												 (not (funcall f x)))))
									(self #'(lambda (f)
											  #'(lambda (x)
												  (funcall (funcall f x) x))))
									(mod #'(lambda (f)
											 #'(lambda (g)
												 #'(lambda (x)
													 (and (funcall f x) (funcall g x))))))
									(ex #'(lambda (f)
											#'(lambda (g)
												(some
												  #'(lambda (x)
													  (and (funcall f x) (funcall g x)))
												  *entity-domain*))))
									))

(defun read-vocab-from-file (vocab-path-string)
  (with-open-file (instr (make-pathname :name vocab-path-string) :direction :input)
    (setf *relation-vocabulary* (read instr nil 'eof))))

(defun process-vocab-item (vocab-item)
  (let ((arity (car vocab-item))
        (names (cdr vocab-item)))
    (if (plusp arity)
      (mapcar 
        #'(lambda (x) (cons x (setutils:generate-tuples *entity-domain* arity)))
        names)
      (mapcar 
        #'(lambda (x) (cons x (setutils:random-pick *entity-domain*)))
        names))))

(defun generate-model ()
    (do ((vocab-list *relation-vocabulary* (cdr vocab-list))
         (model nil (append (process-vocab-item (car vocab-list)) model)))
      ((endp vocab-list) model)))

(defparameter *model* (generate-model))

(defun display-model ()
  (let* ((vocab (sort *relation-vocabulary* #'< :key #'car))
         (arities (mapcar #'car vocab)))
    (format t "~%The domain of entities: ~A~%" *entity-domain*)
    (dolist (arity arities)
      (format t "~%~A-place names: ~%" arity)
      (dolist (name (cdr (assoc arity *relation-vocabulary*)))
        (format t "~A:    ~A~%" name (let ((value (cdr (assoc name *model*))))
									   (if (and (consp value ) (consp (car value)))
										 (mapcar #'reverse value)
										 value)))))))

(defun construct-interpretation ()
  (let ((if-table (make-hash-table)))     ; interpretation function table
	(dolist (item *model*)
	  (let ((vocab-item (car item))       ; vocabulary item
			(interp (cdr item)))         ; set-theoretic interpretation 
		(if (listp interp)
		  (setf (gethash vocab-item if-table) (setutils:set-to-function interp))
		  (setf (gethash vocab-item if-table) interp))))
	(dolist (item *manual-vocabulary* if-table)
	  (let ((vocab-item (car item))    
			(interp (cadr item)))      
		(setf (gethash vocab-item if-table) (eval interp))))))

(defparameter *interpretation* (construct-interpretation))

(defun interpret (form)
  (if (consp form)
	(funcall (interpret (car form)) (interpret (cadr form)))
	(gethash form *interpretation*)))

(defun refresh-model ()
  (setf *model* (generate-model))
  (setf *interpretation* (construct-interpretation)))
