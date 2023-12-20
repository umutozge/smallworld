;;;
;;; Lambda Calculus with quantification
;;;
;;; The following code is an extension of
;;; Alessandro Cimatti's Lambda Calculus implementation 
;;; downloaded from http://slideplayer.com/slide/11363727/
;;; 
;;; Quantification and eta-reduction added.
;;; Umut Ozge Nov 2018
;;; 


; (defpackage :lc-q 
;   (:use :common-lisp)
;   (:export :eta-normalize :beta-normalize-outer :beta-normalize-inner :mk-a))
; 
; (in-package lc-q)


; The ADT for Expressions
; 
; e ::= v | l | a | q
; v ::= symbolp except operator 
; a ::= ( e e ) 
; l ::= ( lam v e ) 
; q ::= ( forall v e ) | ( exists v e )
; 

(defparameter *operators*
  '(lam forall exists))

(defparameter *quantifiers*
  '(forall exists))

; is-X predicates perform top-level check only -- see is-e below for full recognition

(defun mk-v (sym) sym)
(defun is-v (e) (and (symbolp e) (not (member e *operators* :test #'string=)))) 
(defun mk-l (v b) (list 'lam v b))
(defun is-l (e) (and (consp e) (= (length e) 3) (string= 'lam (first e)) (is-v (second e))))
(defun l-get-v (l) (second l))
(defun l-get-b (l) (third l))
(defun mk-a (f a) (list f a))
(defun is-a (e) (and (consp e) (= (length e) 2)))
(defun a-get-f (a) (first a))
(defun a-get-a (a) (second a))
(defun mk-q (q v b) (list q v b))
(defun is-q (e) (and (consp e) (= (length e) 3) (member (car e) *quantifiers* :test #'string=) (is-v (cadr e))))
(defun q-get-q (q) (car q))
(defun q-get-v (q) (cadr q))
(defun q-get-b (q) (caddr q))
(defun freshvar () (gensym))


;;; eta-reduce if possible

(defun eta-reduce (e)
  (if (and
		(is-l e)
		(is-a (l-get-b e))
		(is-v (a-get-a (l-get-b e)))
		(string=
		  (l-get-v e)
		  (a-get-a (l-get-b e))))
	(a-get-f (l-get-b e))
	e))

(defun eta-normalize (e)
  (cond ((is-v e) e)
		((is-a e) (mk-a
					(eta-normalize (a-get-f e))
					(eta-normalize (a-get-a e))))
        ((is-q e) (mk-q
                    (q-get-q e)
                    (q-get-v e)
                    (eta-normalize (q-get-b e))))
		((is-l e)  (eta-reduce (mk-l 
								(l-get-v e)
								(eta-normalize (l-get-b e)))))))
;
; Generalized Recognizer
;
; takes arbitrary s-exp in input 
; 
(defun is-e (e) 
  (cond ((is-v e) t)
		((is-a e) (and (is-e (a-get-f e)) (is-e (a-get-a e)))) 
		((is-l e) (and (is-e (l-get-v e)) (is-e (l-get-b e)))) 
		((is-q e) (and (is-e (q-get-v e)) (is-e (q-get-b e)))) 
		(t nil)))

;
; Free variables 
; Return the free variables of an expression 
;
(defun fv (e) 
  (cond ((is-v e) (list e))
		((is-a e) (append (fv (a-get-f e)) (fv (a-get-a e)))) 
		((is-l e) (remove (l-get-v e) (fv (l-get-b e)))) 
		((is-q e) (remove (q-get-v e) (fv (q-get-b e)))) 
		(t (error "Unknown lambda term type")))) 

(defun free-in (v e) 
  (member v (fv e))) 

;
; Equivalence up to Alpha-conversion
;

(defun alpha-equivalent (e1 e2 &optional rpl1 rpl2) 
  (cond ((is-v e1) 
		 (and (is-v e2)
			  (let ((new1 (cdr (assoc e1 rpl1)))
					(new2 (cdr (assoc e2 rpl2))))
				(if (and (null new1) (null new2))
				  (equal e1 e2)
				  (equal new1 new2))))) 
		((is-a e1) 
		 (and (is-a e2) 
			  (alpha-equivalent (a-get-f e1) (a-get-f e2) rpl1 rpl2)
			  (alpha-equivalent (a-get-a e1) (a-get-a e2) rpl1 rpl2)))
		((is-l e1)
		 (and (is-l e2)
			  (let* ((new (freshvar))
					 (old1 (l-get-v e1))
					 (old2 (l-get-v e2))
					 (newrpl1 (cons (cons old1 new) rpl1))
					 (newrpl2 (cons (cons old2 new) rpl2)))
				(alpha-equivalent (l-get-b e1) (l-get-b e2) newrpl1 newrpl2))))
		((is-q e1)
		 (and (is-q e2)
			  (equal (q-get-q e1) (q-get-q e2))
			  (let* ((new (freshvar))
					 (old1 (q-get-v e1))
					 (old2 (q-get-v e2))
					 (newrpl1 (cons (cons old1 new) rpl1))
					 (newrpl2 (cons (cons old2 new) rpl2)))
				(alpha-equivalent (q-get-b e1) (q-get-b e2) newrpl1 newrpl2))))))

;
; Substitution 
;

(defun subst-with-in (x e1 exp)
  (cond 
	((is-v exp)
	 (if (equal x exp) e1 exp))
	((is-a exp)
	 (mk-a
	   (subst-with-in x e1 (a-get-f exp))
	   (subst-with-in x e1 (a-get-a exp))))
	((is-l exp) ; say exp is (lam y e)
	 (let ((y (l-get-v exp)) (e (l-get-b exp)))
	   (cond
		 ((equal x y) exp)
		 ((not (free-in x e)) exp)
		 ((and (free-in x e) (not (free-in y e1)))
		  (mk-l y (subst-with-in x e1 e)))
		 ((and (free-in x e) (free-in y e1))
		  (let ((z (freshvar)))
			(mk-l z (subst-with-in x e1 (subst-with-in y z e))))))))
	((is-q exp) ; say exp is (q y e)
	 (let ((y (q-get-v exp)) (e (q-get-b exp)) (q (q-get-q exp)))
	   (cond ((equal x y) exp) 
			 ((not (free-in x e)) exp)
			 ((and (free-in x e) (not (free-in y e1)))
			  (mk-q q y (subst-with-in x e1 e)))
			 ((and (free-in x e) (free-in y e1))
			  (let ((z (freshvar)))
				(mk-q q z (subst-with-in x e1 (subst-with-in y z e))))))))))


; Beta reduction

(defun is-rdx (e) (and (is-a e) (is-l (a-get-f e))))
(defun rdx-get-v (rdx) (l-get-v (a-get-f rdx)))
(defun rdx-get-b (rdx) (l-get-b (a-get-f rdx)))
(defun rdx-get-a (rdx) (a-get-a rdx))

; Beta reduce: (a (l v e) e1) ==> [e1 / v] e

(defun beta-reduce (rdx)
  (subst-with-in 
    (rdx-get-v rdx)
    (rdx-get-a rdx)
    (rdx-get-b rdx)))

; Beta reduce if possible

(defun beta-reduce-if-redex (e)
  (if (is-rdx e) (beta-reduce e) e))

; Iterate beta reduction on outermost redex

(defun beta-reduce-outer (e &optional (lim 100))
  (cond
    ((< lim 0) e)
    ((is-rdx e)
     (beta-reduce-outer (beta-reduce-if-redex e) (- lim 1)))
    ((is-v e) e)
    ((is-a e)
     (mk-a
       (beta-reduce-outer (a-get-f e))
       (beta-reduce-outer (a-get-a e))))
    ((is-l e)
     (mk-l
       (l-get-v e)
       (beta-reduce-outer (l-get-b e))))
    ((is-q e)
     (mk-q
       (q-get-q e)
       (q-get-v e)
       (beta-reduce-outer (q-get-b e))))))

; Iterate beta reduction on innermost redex

(defun beta-reduce-inner (e &optional (lim 100))
  (cond
    ((< lim 0) e)
    ((is-v e) e)
    ((is-a e)
     (beta-reduce-if-redex
       (mk-a (beta-reduce-inner (a-get-f e) lim)
	     (beta-reduce-inner (a-get-a e) lim))))
    ((is-l e)
     (mk-l
       (l-get-v e)
       (beta-reduce-inner (l-get-b e) lim)))
    ((is-q e)
     (mk-q
       (q-get-q e)
       (q-get-v e)
       (beta-reduce-inner (q-get-b e) lim)))))

; Beta normalization

(defun beta-normalize-param (e fn &optional (lim 100))
  (let* ((res (apply fn (list e lim)))
	 (use-alpha-equivalent t)
	 (stop (if use-alpha-equivalent
		 (alpha-equivalent res e)
		 (equal res e))))
    (if stop
      res ; fix point reached
      (beta-normalize-param res fn))))

(defun beta-normalize-outer (e &optional (lim 100))
  (beta-normalize-param e 'beta-reduce-outer lim))

(defun beta-normalize-inner (e &optional (lim 100))
  (beta-normalize-param e 'beta-reduce-inner lim))

; try with the two different strategies and compare results

(defun beta-normalize (e)
  (let ((res-inner (beta-normalize-inner e 100))
	(res-outer (beta-normalize-outer e 100)))
    (if (alpha-equivalent res-outer res-inner)
      (progn 
	(format t "Results are alpha equivalent~%")
	(format t "Inner: ~A~%" res-inner)
	(format t "Outer: ~A~2%" res-outer))
      (progn 
	(format t "Results are NOT alpha-equivalent!")
	(format t "Inner: ~A~%" res-inner)
	(format t "Outer: ~A~2%" res-outer)))))


;;; TEST material 

; (defparameter 
;   t1 '(lam x (k x)))
; (defparameter
;   t2 '(lam z (k z)))
; (defparameter
;   loves '(lam x (lam y ((loves x) y))))
; (defparameter
;   john 'john)
; (defparameter 
;   mary 'mary)
; (defparameter 
;   sleeps 'sleep)
; (defparameter 
;   cat 'cat)
; (defparameter
;   def '(lam p (lam q (forall x ((cond (p x)) (q x))))))
; 
; (defparameter
;   q1 '(lam p (exists y (forall x ((p x) y)))))
; 
; (defparameter
;   q2 '(exists z (forall x ((loves x) z))))
; 
; (defparameter
;   bcomb '(lam f (lam g (lam x (f (g x))))))
; 
; (defparameter
;   tcomb '(lam x (lam t (t x))))
; 
