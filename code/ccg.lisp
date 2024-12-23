;;;
;;; A toy implementation of Combinatory Categorial Grammar
;;;
;;; Syntax is driven via term unification with feature matrices
;;; in the style of Pareschi and Steedman (1987).
;;;
;;; Parsing is driven by shift-reduce algorithm.
;;;
;;; Semantics is driven via first-order logic with lambda calculus.
;;;
;;; Feature matrices are represented as a-lists (a set of attribute value pairs).
;;; Variables are symbols with ? prefix.
;;; Atomic values and feature names are not distinguished, they are all symbols.
;;;

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Signs                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A sign is a triple <phon,syn,sem>
;;;
;;; phon: symbol
;;; syn: a feature structure (recursive alist of attribute value pairs)
;;; sem: lambda term

(defstruct sign
  phon
  syn
  sem)

(defstruct lexkey
   (pos '_   :type symbol)
   (phon '_  :type (or symbol integer)))

;;; A way to talk to the lexicon closure

(defmacro lexicon (&body body)
  `(funcall (*state* :lexicon) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Feature Structures    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A category is a feature structure in
;;; the form of an alist, you can think
;;; of it as an attribute value matrice as well.
;;; Categories are read from a lexicon file (default: lexicon.lisp)
;;; E.g.
;;;
;;;  ((cat n) (agr +) (fin -))
;;;

;;; Accessor functions for feature structures

(defun fs-search (fs &rest path)
  "Search a feature structure along the path

   (fs-search '((A B) (K ((C ((D E)))))) 'K 'C 'D ) ==> E

   "
  (labels ((_fs-search (fs path)
             (cond ((endp path) fs)
                   ((atom fs)
                    (if path nil fs))
                   ((feature-structure-p fs)
                    (let ((match (assoc (car path) fs)))
                      (when match
                        (_fs-search (cadr match) (cdr path)))))))
           (feature-structure-p (fs)
             "A superficial checker for feature structures"
             (and (consp fs) (consp (car fs))))
           (attr-val-pair-p (exp)
             (and (consp exp) (= 2 (length exp)))))
    (_fs-search fs path)))

(defun fs-empty-p (fs)
  (endp fs))

(defun fs-value-p (fs)
  (atom fs))

(defun fs-same-attr-p (attrvalA attrvalB)
  (eql (car attrvalA) (car attrvalB)))

(defun fs-same-attrval-p (attrvalA attrvalB)
  (equalp attrvalA attrvalB))

(defun fs-update (fs attrval)
  "a top-level updater"
  (cond ((fs-empty-p fs) (list attrval))
        ((fs-same-attr-p (car fs) attrval) 
         (cons attrval (cdr fs)))
        (t (cons (car fs) (fs-update (cdr fs) attrval)))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Combinators          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; see lc-quan.lisp for the notation for interpretations

(defun a-combine (x y)
  "application"
  (beta-normalize-inner (mk-a x y)))

(defparameter b-comb
  '(lam f (lam g (lam x (f (g x))))))

(defun b-combine (x y &optional (n 1))
  "return (B^n)xy"
  (case n
    (1 (beta-normalize-inner
         (mk-a
           (mk-a
             '(lam f (lam g (lam x (f (g x)))))
             x)
           y)))
    (2 (beta-normalize-inner
         (mk-a
           (mk-a
             '(lam f (lam g (lam x (lam y (f ((g x) y))))))
             x)
           y)))
    (3 (beta-normalize-inner
         (mk-a
           (mk-a
             '(lam f (lam g (lam x (lam y (lam z (f (((g x) y) z)))))))
             x)
           y)))))



(defparameter b-2-comb
  '(lam f (lam g (lam x (lam y (f ((g x) y))))))
  )

(defparameter b-3-comb
  '(lam f (lam g (lam x (lam y (lam z (f (((g x) y) z)))))))
  )

(defparameter t-comb
  '(lam x (lam y (y x))))

(defun t-comb (x)
  "return Tx"
  (beta-reduce-inner
    (mk-a
      '(lam x (lam y (y x)))  
      x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Combination          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (combination)
  left-input
  right-input
  output
  combinator
  direction
  )

(defun get-deco (direction combinator)
  (make-symbol
    (concatenate 'string
                 (let ((name (symbol-name combinator)))
                   (if (string= name "A")
                       ""
                       name))
                 (symbol-name direction))))

(defun combine (left right)
  "return the combination of signs, or nil if not combinable
   Sign -> Sign
   "
  (let ((lsyn (sign-syn left)) (rsyn (sign-syn right)))
    (let ((result
            (or
              (c-apply lsyn rsyn)
              (c-compose lsyn rsyn))))
      (if result
          (let ((syn (first result))
                (direction (second result))
                (combinator (third result)))
            (make-sign
              :phon (cons (get-deco direction combinator)
                          (cons (sign-phon left)
                                (list (sign-phon right))))
              :syn syn
              :sem (s-combine left right direction combinator)))))))


;;; Application

(defun c-apply (lsyn rsyn)
  "combine sign-syn's  with application, if possible
   return (sign-syn direction combinator) or NIL"
  (or
    (and (eq 'forward (fs-search lsyn 'slash 'dir))
         (let ((result (_apply lsyn rsyn)))
           (when result
             (list result '> 'a))))
    (and (eq 'backward  (fs-search rsyn 'slash 'dir))
         (let ((result (_apply rsyn lsyn)))
           (when result
             (list result '< 'a))))))

(defun _apply (func arg)
  (multiple-value-bind (bindings yes) (unifier:tree-match (fs-search func 'in) arg)
	(if yes
	  (sublis bindings (fs-search func 'out)))))

;;; Mode check

(defun modes-afforded (m1 m2)
  (intersection 
    (gethash m1 (*state* :mode-table))
    (gethash m2 (*state* :mode-table))))

;;; Composition

(defun c-compose (lsyn rsyn) ; TODO generalized composition (make order a parameter, perhaps?)
  "combine sign-syn's  with composition, if possible
   return (sign-syn direction combinator) or NIL"
  (let ((modes (apply #'modes-afforded (mapcar
                                         #'(lambda (syn)
                                             (fs-search syn 'slash 'mode))
                                         (list lsyn rsyn)))))
    (or
      (and (eql 'forward (fs-search lsyn 'slash 'dir))
           (or
             (and
               (eql 'forward (fs-search rsyn 'slash 'dir))
               (member 'harmonic modes))
             (and 
               (eql 'backward (fs-search rsyn 'slash 'dir))
               (member 'cross modes)))
           (let ((result (_compose lsyn rsyn)))
             (if result
                 (list result '> 'b))))
      (and (eql 'backward (fs-search rsyn 'slash 'dir))
           (or
             (and
               (eql 'backward (fs-search lsyn 'slash 'dir))
               (member 'harmonic modes))
             (and 
               (eql 'forward (fs-search lsyn 'slash 'dir))
               (member 'cross modes)))
           (let ((result (_compose rsyn lsyn)))
             (if result
                 (list result '< 'b)))))))

(defun _compose (f g)
  "combine the cats with composition, if possible"
  (multiple-value-bind (bindings yes) (unifier:tree-match (fs-search f 'in) (fs-search g 'out))
    (if yes
        (list
          (cons 'in
                (list (sublis bindings (fs-search g 'in))))
          (list 'slash (list (list 'dir (fs-search g 'slash 'dir)) (list 'mode (fs-search g 'slash 'mode))))
          (cons 'out
                (list (sublis bindings (fs-search f 'out))))))))

;;; Semantic composition

(defun s-combine (left right dir combinator)
  (labels ((s-apply (left right dir)
             "handle the semantic side of application"
             (let ((lsem (sign-sem left))
                   (rsem (sign-sem right)))
               (if (eq dir '>)
                   (a-combine lsem rsem)
                   (a-combine rsem lsem))))
           (s-compose (left right dir)
             "handle the semantic side of composition"
             (let ((lsem (sign-sem left))
                   (rsem (sign-sem right)))
               (if (eq dir '>)
                   (b-combine lsem rsem)
                   (b-combine rsem lsem)))))
    (case combinator
      (a (s-apply left right dir))
      (b (s-compose left right dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            Parsing           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An enumeration is a list of signs ordered in surface order.
;;; Due to lexical ambiguity a surface form may have more than one enumeration.
;;; So the first task is to map a surface form (ordered list of (pos phon)) to a set
;;; of enumerations.


(defun enumerate (input)
  "
  Input: list of symbols entered at :p prompt.
  Output: list of all possible enumerations, each a list of signs.

  Enumerator works with and without (*state* :morphology)
  "

  (labels ((generate-enums (sentence &optional (enums '(nil)))
             "map a list of (pos phon) pairs to a set of enumerations,
              where each enumeration is a sequence of signs."
             (if (endp sentence)
                 enums
                 (generate-enums
                   (cdr sentence)
                   (let ((entries (handler-case (lexicon (car sentence))
                                    (SIMPLE-ERROR (e)
                                                  (error
                                                    (make-condition 'item-not-found :lexkey (car sentence))))
                                    )))
                     (mapcan
                       #'(lambda (x)
                           (mapcar
                             #'(lambda (y)
                                 (append x (list y)))
                             entries))
                       enums)))))

           (morph-parse (expression)
             "string -> ((lexkeys)s)

              Input: string
              Output: list of list of pairs
              "
             (let ((result (typecase expression 
                             (simple-base-string
                               (funcall (*state* :morph-analyzer) expression))
                             (symbol
                               (funcall (*state* :morph-analyzer) (str:downcase (symbol-name expression)))))))
               (if (some
                     #'(lambda (item)
                         (some
                           #'(lambda (lexkey)
                               (equalp (lexkey-phon lexkey) '+?))
                           item))
                     result)
                   (error (make-condition 'no-morph-parse :input expression)) 
                   result)))
           
           )

      (if (*state* :morphology)
          (reduce
            #'append
            (mapcar
              #'generate-enums
              (with-debug (aux:cartesian-product
                            (mapcar #'morph-parse input))
                          :message "Morph parses:"
                          :transform #'(lambda (parse-list)
                                         (mapcar
                                           #'(lambda (parse)
                                               (mapcar
                                                 #'(lambda (lexkey)
                                                     (list (lexkey-phon lexkey) (lexkey-pos lexkey)))
                                                 parse))
                                           parse-list)))))
          (generate-enums
            (mapcar
              #'(lambda (x)
                  (make-lexkey :phon x))
              input)))))

(defun parse (expression)
  (mapcar
    #'(lambda (x)
        (setf (sign-sem (car x)) (funcall (if (*state* :eta-normalize)  #'eta-normalize 'identity) (sign-sem (car x))))
        x)
    (mapcan
      #'(lambda (x)
          (sr-parser:parse (list (cons nil x)) #'combine))
      (with-debug
        (enumerate expression)
        :message "Enumerations:"
        :transform #'(lambda (enumerations) (aux:maptree #'(lambda (x) (pretty-print :type :sign :format :text :form x)) enumerations))))))

;;; An equality predicate for semantic interpretations

(defun sign-sem-equalp (s1 s2)
  "two signs are semantically equivalent, if their sems are alpha-equivalent"
  (alpha-equivalent (sign-sem s1) (sign-sem s2)))

(defun uniq-parses (set-of-parses)
  "eliminates semantically spurious parses -- see aux.lisp for aux:uniq"
  (aux:uniq set-of-parses #'(lambda (x y) (sign-sem-equalp (car x) (car y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Lexicon Management        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-lexicon ()
  "Loads lexical items fetched by read-lexicon to aux:multi-set-table pointed to by (*state* :lexicon).

   Keys to the lexicon are (pos phon)"

  (labels ((push-item (item)
             "item is an alist: (key pos phon syn sem)"
             (let* ((sign (construct-sign item))
                    (pos (cadr (assoc 'pos item)))
                    (phon (sign-phon sign)))
               (lexicon (make-lexkey :pos pos :phon phon) sign)))
           (construct-sign (lex)
             (make-sign :phon (cadr (assoc 'phon lex))
                        :syn (unifier:refresh-vars (cadr (assoc 'syn lex)))
                        :sem (cadr (assoc 'sem lex)))))

    (do ((count 0 (+ 1 count))
          (items
            (handler-case (read-lexicon)
              (SB-KERNEL::ARG-COUNT-ERROR (e)
                                          (format t "~%~%~A***BROKEN LEXICON, REVISE and :RELOAD.***~%~%" #\Tab ))
              (INVALID-FEATURE-VALUE (e)
                                     (format t "~%~%LEXICON ERROR: ~A is an invalid value for the feature ~A, revise and :reload.~%" (value e) (feature e)))
              (DEFAULT-FEATURE-OVERRIDE (e)
                                        (format t "~%~%LEXICON ERROR: ~A attempts to override the default ~A, revise and :reload.~%" (overrider e) (default e)))
              (BAD-SYNTACTIC-TYPE (e)
                                  (format t "~%~%LEXICON ERROR: Bad syntactic type: ~A, revise and :reload.~%" (definition e)))
              (BAD-SEMANTIC-INTERPRETATION (e)
                                  (format t "~%~%LEXICON ERROR: Bad semantic interpretation: ~A, revise and :reload.~%" (definition e))))
            (cdr items)))
        ((endp items) (progn (lexicon :keys) count))
        (push-item (car items)))))
