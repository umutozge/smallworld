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

;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(load "lc-q.lisp")
(load "aux.lisp")
(load "lexicon-parser.lisp")
(load "unifier.lisp")

; (defpackage :uni-cg
;   (:use :common-lisp)
;   (:export :parse
;            :uniq-parses
;            :init-parser
;            :sign-sem
; 		   ))

; (in-package uni-cg)

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


(defun get-dir (fs)
  "return the directionality of a functor -- nil if not a functor"
  (search-syn fs 'slash 'dir))

(defun get-mode (fs)
  "return the mode a functor -- nil if not a functor"
  (search-syn fs 'slash 'mode))

(defun get-output (fs)
  "return the output of a functor -- nil if not a functor"
  (search-syn fs 'out))

(defun get-input (fs)
  "return the input of a functor -- nil if not a functor"
  (search-syn fs 'in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Lexicon Management        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A functional wrappaer to interact with the lexicon

(defun lexicon (&rest input)
  (apply (*state* :lexicon) input))


(defun read-lexicon (&key (path (*state* :debug-lexicon-path)))
  "Reads lexical entries from path, parses them into signs, and stores them in a aux:multi-set-table (see aux.lisp).
   the global var (*state* :lexicon) ends up pointing to a closure where one can add, get or 
   query lexical entries using the top level phon feature.
   The closure (*state* :lexicon) is a mapping from phon to the list of signs associated with that phon"
  (format t "~%Reading lexicon...")
  (with-open-file (instr path :direction :input)
    (format t "Read ~A items.~%"
            (do ((count 0 (+ 1 count))
                 (item
                   (read instr nil 'eof)
                   (read instr nil 'eof)))
                ((eq item 'eof) count)
                (let ((sign (construct-sign item)))
                  (lexicon (sign-phon sign) sign))))))

(defun construct-sign (lex)
  (make-sign :phon (cadr (assoc 'phon lex))
             :syn (unifier:refresh-vars (cadr (assoc 'syn lex)))
             :sem (cadr (assoc 'sem lex))))

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
  "return the combination of signs, or nil if not combinable"
  (let ((lsyn (sign-syn left)) (rsyn (sign-syn right)))
    (let ((result
             (or
               (c-apply lsyn rsyn)
               (c-compose lsyn rsyn))))
      (if result
          (let ((syn (first result))
                (direction (second result))
                (combinator (third result)))
           (make-combination
             :left-input left
             :right-input right
             :output (make-sign
                       :phon (cons (get-deco direction combinator) (cons (sign-phon left) (list (sign-phon right))))
                       :syn syn
                       :sem (s-combine left right direction combinator))
             :direction direction
            ))))))


;;; Application

(defun c-apply (lsyn rsyn)
  "combine the cats with application, if possible"
  (or
    (and (eq 'forward (get-dir lsyn))
         (let ((result (_apply lsyn rsyn)))
           (when result
             (list result '> 'a))))
    (and (eq 'backward (get-dir rsyn))
         (let ((result (_apply rsyn lsyn)))
           (when result
             (list result '< 'a))))))

(defun _apply (func arg)
  (multiple-value-bind (bindings yes) (unifier:tree-match (get-input func) arg)
	(if yes
	  (sublis bindings (get-output func)))))


;;; Composition

(defun c-compose (lsyn rsyn)
  "combine the cats with composition, if possible"
  (or
    (and (eql 'forward (get-dir lsyn))
         (eql 'dot (get-mode lsyn))
         (let ((result (_compose lsyn rsyn)))
           (if result
               (list result '> 'b))))
    (and (eql 'backward (get-dir rsyn))
         (eql 'dot (get-mode rsyn))
         (let ((result (_compose rsyn lsyn)))
           (if result
               (list result '< 'b))))))

(defun _compose (f g)
  "combine the cats with composition, if possible"
  (multiple-value-bind (bindings yes) (unifier:tree-match (get-input f) (get-output g))
    (if yes
        (list
          (cons 'in
                (list (sublis bindings (get-input g))))
          (cons 'slash (list (list 'dir (get-dir g)) (list 'mode (get-mode g))))
          (cons 'out
                (list (sublis bindings (get-output f))))))))

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

;;; An enumeration is a list of signs ordered in surface order
;;; due to lexical ambiguity a surface form may have more than one enumeration
;;; So the first task is to map a surface form (ordered list of phons) to a set
;;; of enumerations

(defun generate-enums (sentence &optional (enums '(nil)))
  "map sentence (list of phons) to a set of enumerations"
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

;;; The shift-reduce parser works with a list of input signs and a
;;; stack of stacks.


;;; A parser state is a pair of a stack and input-tape.
;;; Define two actions shift and reduce that works on parser states


(defun p-shift (state)
  "shift the top symbol on tape to stack"
  (let ((stack (car state))
        (tape (cdr state)))
    (if (null tape)
        (error "Shift attempt for empty tape")
        (cons
          (cons (car tape) stack)
          (cdr tape)))))

(defun p-reduce (state)
  "attempt to combine top two items in stack; raise error if stack is too short; return nil for combination failure"
  (let ((stack (car state))
        (tape (cdr state)))
    (if (< (length stack) 2)
        (error "Reduce attempt on a short stack")
        (let ((reduct (combine (cadr stack) (car stack))))
          (if reduct
              (cons
                (cons (combination-output reduct) (cddr stack))
                tape))))))

(defun p-success (state)
  (let ((stack (car state))
        (tape (cdr state)))
    (and
      (null tape)
      (consp stack)
      (null (cdr stack))
      (car stack))))

(defun p-empty-tapep (state)
  (endp (cdr state)))

(defun p-reducible-statep (state)
  (> (length (car state)) 1))



;;; a shift-reduce parser is a queue of parser states (agenda)
;;; car of the agenda is the current state


(defun sr-parse (agenda &optional store)
  (if (endp agenda)
      store
      (let* ((state (car agenda)))
        (cond ((p-success state)
               (sr-parse (cdr agenda)
                         (cons (caar state) store)))
              ((p-empty-tapep state) ;tape is over
               (if (p-reducible-statep state) ; is stack still reducible
                   (sr-parse                    ; yes, then reduce it
                     (cons (p-reduce state) (cdr agenda))
                     store)
                   (sr-parse (cdr agenda) store))) ; no, discard the curren state and go on
              ((not (p-reducible-statep state)) ; compulsory shift
               (sr-parse (cons (p-shift state) (cdr agenda)) store))
              (t
               (let ((reduct (p-reduce state))
                     (shifted (p-shift state)))
                 (if reduct
                     (sr-parse (cons
                                 reduct
                                 (cons shifted (cdr agenda)))
                               store)
                     (sr-parse (cons
                                 shifted
                                 (cdr agenda))
                               store))))))))

(defun parse (sentence)
  (mapcar
    #'(lambda (x)
        (setf (sign-sem x) (funcall (if (*state* :eta-normalize)  #'eta-normalize 'identity) (sign-sem x)))
        x)
    (mapcan
      #'(lambda (x)
          (sr-parse (list (cons nil x))))
      (generate-enums sentence))))

;;; An equality predicate for semantic interpretations

(defun sign-sem-equalp (s1 s2)
  "two signs are semantically equivalent, if their sems are alpha-equivalent"
  (alpha-equivalent (sign-sem s1) (sign-sem s2)))

(defun uniq-parses (set-of-parses)
  "eliminates semantically superous parses -- see aux.lisp for aux:uniq"
  (aux:uniq set-of-parses #'sign-sem-equalp))

;;;
;;; Initialization
;;;

(defun init-parser ()
  (lexicon-parser:parse-lexicon)
  (read-lexicon))
