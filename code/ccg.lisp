;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Combinators          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; see lc-quan.lisp for the notation for interpretations 

(defparameter b-comb
  '(lam f (lam g (lam x (f (g x))))))

(defparameter t-comb
  '(lam x (lam y (y x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Combination          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine (left right)
  "return the combination of signs, or nil if not combinable"
  (let ((lsyn (sign-syn left)) (rsyn (sign-syn right)))
	(or (let ((result (c-apply lsyn rsyn)))
		  (if result
			(make-sign 
			  :phon (cons (sign-phon left) (sign-phon right))
			  :syn (cadr result)
			  :sem (s-apply left right (car result)))))
		(let ((result (c-compose lsyn rsyn)))
		  (if result
			(make-sign 
			  :phon (cons (sign-phon left) (sign-phon right))
			  :syn (cadr result)
			  :sem (s-compose left right (car result))))))))


;;; Application

(defun c-apply (lsyn rsyn)
  "combine the cats with application, if possible"
  (or
		(and (eq 'forward (get-dir lsyn))
				 (let ((result (_apply lsyn rsyn)))
					 (when result
						 (list 'forward result))))
		(and (eq 'backward (get-dir rsyn))
				 (let ((result (_apply rsyn lsyn)))
					 (when result
						 (list 'backward result))))))

(defun _apply (func arg)
	(multiple-value-bind (bindings yes) (unifier:tree-match (get-input func) arg)
		(if yes 
			(sublis bindings (get-output func)))))

(defun s-apply (left right dir)
  "handle the semantic side of application"
	(let ((lsem (sign-sem left))
			 (rsem (sign-sem right)))
		(if (eq dir 'forward)
			(beta-normalize-inner (mk-a lsem rsem))
			(beta-normalize-inner (mk-a rsem lsem)))))

;;; Composition

(defun c-compose (lsyn rsyn)
	"combine the cats with composition, if possible"
	(or
		(and (eq 'forward (get-dir lsyn))
				 (let ((result (_compose lsyn rsyn)))
					 (if result
					(list 'forward result))))
		(and (eq 'backward (get-dir rsyn))
				 (let ((result (_compose rsyn lsyn)))
					 (if result
					(list 'backward result))))))

(defun _compose (f g)
  "combine the cats with composition, if possible"
	(multiple-value-bind (bindings yes) (unifier:tree-match (get-input f) (get-output g))
		(if yes 
			(list 
				(cons 'in 
							(list (sublis bindings (get-input g))))
				(cons 'dir (list (get-dir g)))
				(cons 'out 
							(list (sublis bindings (get-output f))))))))

(defun s-compose (left right dir)
	"handle the semantic side of composition"
	(let ((lsem (sign-sem left))
			 (rsem (sign-sem right)))
		(if (eq dir 'forward)
			(beta-normalize-inner (mk-a (mk-a b-comb lsem) rsem))
			(beta-normalize-inner (mk-a (mk-a b-comb rsem) lsem)))))

