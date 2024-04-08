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
   (cat '_   :type symbol)
   (phon '_  :type (or symbol integer)))

;;; A way to talk to the lexicon closure

(defmacro lexicon (&body body)
  `(funcall (*state* :lexicon) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    (Syntactic) Categories    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A category is a feature structure in
;;; the form of an alist, you can think
;;; of it as an attribute value matrice as well.
;;; Categories are read from a lexicon file (default: lexicon.lisp)
;;; E.g.
;;;
;;;  ((cat n) (agr +) (fin -))
;;;

;;; Accessor functions for feature structures

(defun search-syn (fs &rest path)
  "Search a feature structure along the path

   (search-syn '((A B) (K ((C ((D E)))))) 'K 'C 'D ) ==> E

   "
  (labels ((s-syn (fs path)
             (cond ((endp path) fs)
                   ((atom fs)
                    (if path nil fs))
                   ((feature-structure-p fs)
                    (let ((match (assoc (car path) fs)))
                      (when match
                        (s-syn (cadr match) (cdr path)))))))
           (feature-structure-p (fs)
             "A superficial checker for feature structures"
             (and (consp fs) (consp (car fs))))
           (attr-val-pair-p (exp)
             (and (consp exp) (= 2 (length exp)))))
    (s-syn fs path)))


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
    (and (eq 'forward (search-syn lsyn 'slash 'dir))
         (let ((result (_apply lsyn rsyn)))
           (when result
             (list result '> 'a))))
    (and (eq 'backward  (search-syn rsyn 'slash 'dir))
         (let ((result (_apply rsyn lsyn)))
           (when result
             (list result '< 'a))))))

(defun _apply (func arg)
  (multiple-value-bind (bindings yes) (unifier:tree-match (search-syn func 'in) arg)
	(if yes
	  (sublis bindings (search-syn func 'out)))))

;;; Composition

(defun c-compose (lsyn rsyn) ; TODO generalized composition
  "combine sign-syn's  with composition, if possible
   return (sign-syn direction combinator) or NIL"
  (or
    (and (eql 'forward (search-syn lsyn 'slash 'dir))
         (eql 'dot (search-syn lsyn 'slash 'mode)) ; TODO implement modal hierarchy
         (let ((result (_compose lsyn rsyn)))
           (if result
               (list result '> 'b))))
    (and (eql 'backward (search-syn rsyn 'slash 'dir))
         (eql 'dot (search-syn rsyn 'slash 'mode))
         (let ((result (_compose rsyn lsyn)))
           (if result
               (list result '< 'b))))))

(defun _compose (f g)
  "combine the cats with composition, if possible"
  (multiple-value-bind (bindings yes) (unifier:tree-match (search-syn f 'in) (search-syn g 'out))
    (if yes
        (list
          (cons 'in
                (list (sublis bindings (search-syn g 'in))))
          (list 'slash (list (list 'dir (search-syn g 'slash 'dir)) (list 'mode (search-syn g 'slash 'mode))))
          (cons 'out
                (list (sublis bindings (search-syn f 'out))))))))

;;; Semantic composition

(defun s-combine (left right dir combinator)
  (labels ((s-apply (left right dir)
             "handle the semantic side of application"
             (let ((lsem (sign-sem left))
                   (rsem (sign-sem right)))
               (if (eq dir '>)
                   (beta-normalize-inner (mk-a lsem rsem))
                   (beta-normalize-inner (mk-a rsem lsem)))))
           (s-compose (left right dir)
             "handle the semantic side of composition"
             (let ((lsem (sign-sem left))
                   (rsem (sign-sem right)))
               (if (eq dir '>)
                   (beta-normalize-inner (mk-a (mk-a b-comb lsem) rsem))
                   (beta-normalize-inner (mk-a (mk-a b-comb rsem) lsem))))))
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

(defun morph-parse (expression)
  "string -> ((lexkeys)s)

   Input: string
   Output: list of list of pairs
  "
  (labels ((wrap-string-in-parentheses (str)
             (concatenate 'string "(" str ")"))
           (swap-pairs (list-of-pairs)
             (mapcar
               #'(lambda (pair)
                (make-lexkey :cat (second pair) :phon (first pair)))
               list-of-pairs)))
    (mapcar
      #'(lambda (x)
          (swap-pairs
            (aux:partition
              (read-from-string
                (wrap-string-in-parentheses x))
              2))) 
      (typecase expression 
        (simple-base-string (flookup expression))
        (symbol (flookup (str:downcase (symbol-name expression))))))))

(defun enumerate (input)

  (labels ((generate-enums (sentence &optional (enums '(nil)))
             "map a list of (pos phon) pairs to a set of enumerations,
              where each enumeration is a sequence of signs."
             (if (endp sentence)
                 enums
                 (generate-enums
                   (cdr sentence)
                   (let ((entries (lexicon (car sentence))))
                     (mapcan
                       #'(lambda (x)
                           (mapcar
                             #'(lambda (y)
                                 (append x (list y)))
                             entries))
                       enums)))))
           ) 

      (if (*state* :morphology)
          (aux:cartesian-product
            (mapcar 
              #'morph-parse
              input))
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
      (enumerate expression))))

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
                    (cat (cadr (assoc 'cat item)))
                    (phon (sign-phon sign)))
               (lexicon (make-lexkey :cat cat :phon phon) sign)))
           (construct-sign (lex)
             (make-sign :phon (cadr (assoc 'phon lex))
                        :syn (unifier:refresh-vars (cadr (assoc 'syn lex)))
                        :sem (cadr (assoc 'sem lex)))))

    (do ((count 0 (+ 1 count))
          (items (read-lexicon) (cdr items)))
        ((endp items) (progn (lexicon :keys) count))
        (push-item (car items)))))
