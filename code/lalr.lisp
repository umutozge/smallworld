;;;
;;; LALR(1) parser generator for SmallWorld.
;;;
;;; Adapted from Mark Johnson's 1988 LALR(1) generator. The original was
;;; structured around module-level globals (glex, grules, gstart, gstarts,
;;; gcats, gepsilons, gexpansions, stateList, nextStateNo) which forced
;;; SmallWorld to ship two near-identical copies of the file -- one in
;;; package SYN-PARSER and one in package SEM-PARSER -- so that the syn
;;; and sem parsers wouldn't clobber each other's globals.
;;;
;;; This version threads a GRAMMAR struct through the generator so we
;;; can build any number of parsers from a single package, and it returns
;;; a parser as a function (a closure over its state machine) instead of
;;; evaluating a top-level DEFUN.
;;;
;;; External interface:
;;;
;;;   (lalr:make-parser RULES LEX END-MARKER)
;;;     -> a function (NEXT-INPUT PARSE-ERROR) -> parse-tree
;;;
;;;   (lalr:run PARSER TOKENS)
;;;     convenience: feed a list of (cat . val) cons cells.
;;;
;;; The parser-function semantics are the same as the original
;;; LALR-PARSER: NEXT-INPUT returns a CONS (category . value) per call,
;;; ending with (END-MARKER . _); PARSE-ERROR is called on a syntax error.
;;;

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defpackage :lalr
  (:use :common-lisp)
  (:export :make-parser :run))

(in-package :lalr)

(defconstant +top-cat+ '$Start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar object -- replaces the file-level globals in the original.

(defstruct grammar
  end-marker
  lex
  start
  rules
  cats
  starts
  epsilons
  expansions
  states
  (next-state-no -1))

(defstruct rule no mother daughters action)
(defstruct state name citems shifts conflict)
(defstruct shift cat where)
(defstruct item rule pos la)

;;; rule / item helpers (used to be macros over globals; now plain accessors)

(defun item-daughters (i) (rule-daughters (item-rule i)))
(defun item-right (i) (nthcdr (item-pos i) (item-daughters i)))

(defun item-equal (i1 i2)
  (and (eq (item-rule i1) (item-rule i2))
       (= (item-pos i1) (item-pos i2))
       (eq (item-la i1) (item-la i2))))

(defun item-core-equal (c1 c2)
  (and (eq (item-rule c1) (item-rule c2))
       (= (item-pos c1) (item-pos c2))))

(defun expand (g cat)
  (cdr (assoc cat (grammar-expansions g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar setup

(defun coerce-action (action)
  "Rule actions in SmallWorld grammars are written as #'(lambda ...).
   Inside a quoted grammar list those read as (FUNCTION (LAMBDA ...)),
   not real function objects.  The original generator side-stepped this
   by EVAL'ing the entire emitted parser; we coerce explicitly here
   instead so that callers can pass either a function object, a symbol,
   or a sharp-quoted lambda form."
  (cond ((functionp action) action)
        ((symbolp action) action)
        ((and (consp action) (eq (car action) 'function))
         (coerce (cadr action) 'function))
        ((and (consp action) (eq (car action) 'lambda))
         (coerce action 'function))
        (t (error "lalr: don't know how to use ~S as a rule action" action))))

(defun transform-rule (raw no)
  (make-rule :no no
             :mother (first raw)
             :daughters (butlast (cddr raw))
             :action (coerce-action (car (last raw)))))

(defun compute-expansion (g cat)
  (remove-if-not (lambda (r) (eq (rule-mother r) cat))
                 (grammar-rules g)))

(defun get-all-cats (g)
  (labels ((try (deja-vu cat)
             (if (find cat deja-vu)
                 deja-vu
                 (try-rules (cons cat deja-vu) (compute-expansion g cat))))
           (try-rules (deja-vu rules)
             (if rules
                 (try-rules (try-cats deja-vu (rule-daughters (car rules)))
                            (cdr rules))
                 deja-vu))
           (try-cats (deja-vu cats)
             (if cats
                 (try-cats (try deja-vu (car cats)) (cdr cats))
                 deja-vu)))
    (try '() (grammar-start g))))

(defun derives-eps (g c)
  "T if c can be rewritten as the null string"
  (labels ((try (deja-vu cat)
             (unless (find cat deja-vu)
               (some (lambda (r)
                       (every (lambda (c1) (try (cons cat deja-vu) c1))
                              (rule-daughters r)))
                     (expand g cat)))))
    (try '() c)))

(defun derives-epsilon (g c)
  (member c (grammar-epsilons g)))

(defun first-terms (g cat-list)
  "the leading terminals of an expansion of cat-list"
  (labels ((first-ds (cats)
             (if cats
                 (if (derives-epsilon g (car cats))
                     (cons (car cats) (first-ds (cdr cats)))
                     (list (car cats)))))
           (try (deja-vu cat)
             (if (member cat deja-vu)
                 deja-vu
                 (try-list (cons cat deja-vu)
                           (mapcan (lambda (r) (first-ds (rule-daughters r)))
                                   (expand g cat)))))
           (try-list (deja-vu cats)
             (if cats
                 (try-list (try deja-vu (car cats)) (cdr cats))
                 deja-vu)))
    (remove-if-not (lambda (term)
                     (or (eq (grammar-end-marker g) term)
                         (find term (grammar-lex g))))
                   (try-list '() (first-ds cat-list)))))

(defun first-terminals (g cat-list)
  (if cat-list
      (if (derives-epsilon g (first cat-list))
          (union (cdr (assoc (first cat-list) (grammar-starts g)))
                 (first-terminals g (rest cat-list)))
          (cdr (assoc (first cat-list) (grammar-starts g))))
      '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LALR(1) state machine construction

(defun close-items (g items)
  "computes the closure of a set of items"
  (do ((todo items))
      ((null todo) items)
    (let ((i (pop todo)))
      (when (item-right i)
        (dolist (la (first-terminals g
                                     (append (rest (item-right i))
                                             (list (item-la i)))))
          (dolist (r (expand g (first (item-right i))))
            (unless (dolist (i items)
                      (when (and (eq r (item-rule i))
                                 (= (item-pos i) 0)
                                 (eq (item-la i) la))
                        (return t)))
              (let ((new (make-item :rule r :pos 0 :la la)))
                (push new items)
                (push new todo)))))))))

(defun shift-items (items cat)
  "shifts a set of items over cat"
  (let ((new-items '()))
    (dolist (i items)
      (when (eq (first (item-right i)) cat)
        (push (make-item :rule (item-rule i)
                         :pos (1+ (item-pos i))
                         :la (item-la i))
              new-items)))
    new-items))

(defun items-right (items)
  "returns the set of categories appearing to the right of the dot"
  (let ((right '()))
    (dolist (i items)
      (let ((d (first (item-right i))))
        (when (and d (not (find d right)))
          (push d right))))
    right))

(defun compact-items (items)
  "collapses items with the same core to compact items"
  (let ((so-far '()))
    (dolist (i items)
      (let ((ci (dolist (s so-far)
                  (when (item-core-equal s i) (return s)))))
        (if ci
            (push (item-la i) (item-la ci))
            (push (make-item :rule (item-rule i)
                             :pos (item-pos i)
                             :la (list (item-la i)))
                  so-far))))
    (sort so-far #'< :key (lambda (i) (rule-no (item-rule i))))))

(defun expand-citems (citems)
  (let ((items '()))
    (dolist (ci citems)
      (dolist (la (item-la ci))
        (push (make-item :rule (item-rule ci)
                         :pos (item-pos ci)
                         :la la)
              items)))
    items))

(defun subsumes-citems (ci1s ci2s)
  (and (= (length ci1s) (length ci2s))
       (every (lambda (ci1 ci2)
                (and (item-core-equal ci1 ci2)
                     (subsetp (item-la ci1) (item-la ci2))))
              ci1s ci2s)))

(defun merge-citems (ci1s ci2s)
  "Adds the las of ci1s to ci2s. ci2s should subsume ci1s."
  (mapcar (lambda (ci1 ci2)
            (setf (item-la ci2) (nunion (item-la ci1) (item-la ci2))))
          ci1s ci2s)
  ci2s)

(defun lookup-state (g citems)
  (dolist (state (grammar-states g))
    (when (and (= (length citems) (length (state-citems state)))
               (do ((ci1s citems (cdr ci1s))
                    (ci2s (state-citems state) (cdr ci2s)))
                   ((null ci1s) t)
                 (unless (item-core-equal (car ci1s) (car ci2s))
                   (return nil))))
      (return state))))

(defun add-state (g citems)
  (let ((new (make-state
               :name (intern (format nil "STATE-~D"
                                     (incf (grammar-next-state-no g))))
               :citems citems)))
    (push new (grammar-states g))
    new))

(defun get-state-name (g items)
  (let* ((citems (compact-items items))
         (state (lookup-state g citems)))
    (cond ((null state)
           (setf state (add-state g citems))
           (build-state g state items))
          ((subsumes-citems citems (state-citems state))
           nil)
          (t
           (merge-citems citems (state-citems state))
           (follow-state g items)))
    (state-name state)))

(defun build-state (g state items)
  (let ((closure (close-items g items)))
    (dolist (cat (items-right closure))
      (push (make-shift :cat cat
                        :where (get-state-name g (shift-items closure cat)))
            (state-shifts state)))))

(defun follow-state (g items)
  (let ((closure (close-items g items)))
    (dolist (cat (items-right closure))
      (get-state-name g (shift-items closure cat)))))

(defun build-table (g)
  (setf (grammar-states g) '()
        (grammar-next-state-no g) -1)
  (get-state-name g
                  (list (make-item
                          :rule (make-rule :no 0
                                           :mother +top-cat+
                                           :daughters (list (grammar-start g)))
                          :pos 0
                          :la (grammar-end-marker g))))
  (setf (grammar-states g) (nreverse (grammar-states g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser-function construction
;;;
;;; The original generator emitted Lisp source for a top-level
;;; (DEFUN LALR-PARSER ...) and relied on EVAL.  Here we instead build
;;; an interpreter loop that consults the state list directly.
;;; Parsing time is dominated by user actions and unification, so the
;;; small overhead of dispatching states by name (vs. compiled CASE) is
;;; negligible -- and avoiding EVAL keeps the code package-clean.

(defun translate-state-table (g)
  "Translate states into a hash table keyed by state-name; each entry is
   a plist (:shifts ((cat . where) ...) :reduces ((la-list pos action mother) ...))."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (state (grammar-states g))
      (let* ((reduces-raw
               (compact-items
                 (delete-if #'item-right
                            (close-items g
                                         (expand-citems (state-citems state))))))
             (symbols-so-far '())
             (shifts (mapcar (lambda (sh)
                               (push (shift-cat sh) symbols-so-far)
                               (cons (shift-cat sh) (shift-where sh)))
                             (state-shifts state)))
             (reduces
               (mapcar (lambda (item)
                         (when (intersection (item-la item) symbols-so-far)
                           (warn "Not LALR(1): ~A, ~A --> ~{~A ~}"
                                 (state-name state)
                                 (rule-mother (item-rule item))
                                 (rule-daughters (item-rule item)))
                           (setf (item-la item)
                                 (nset-difference (item-la item)
                                                  symbols-so-far)))
                         (dolist (la (item-la item))
                           (push la symbols-so-far))
                         (list (item-la item)
                               (item-pos item)
                               (rule-action (item-rule item))
                               (rule-mother (item-rule item))))
                       reduces-raw)))
        (setf (gethash (state-name state) table)
              (list :shifts shifts :reduces reduces))))
    table))

(defun make-parser (rules lex end-marker)
  "Build an LALR(1) parser from RULES, a list of LEX terminal categories,
   and an END-MARKER. Return a closure (NEXT-INPUT PARSE-ERROR)."
  (let* ((g (make-grammar :end-marker end-marker
                          :lex lex
                          :start (caar rules))))
    (setf (grammar-rules g)
          (let ((i 0))
            (mapcar (lambda (r) (transform-rule r (incf i))) rules)))
    (setf (grammar-cats g) (get-all-cats g))
    (setf (grammar-expansions g)
          (mapcar (lambda (cat) (cons cat (compute-expansion g cat)))
                  (grammar-cats g)))
    (setf (grammar-epsilons g) (remove-if-not (lambda (c) (derives-eps g c))
                                              (grammar-cats g)))
    (setf (grammar-starts g)
          (cons (list end-marker end-marker)
                (mapcar (lambda (cat) (cons cat (first-terms g (list cat))))
                        (grammar-cats g))))
    (build-table g)
    (let ((table (translate-state-table g))
          (start-state (state-name (first (grammar-states g))))
          (top-cat +top-cat+))
      (lambda (next-input parse-error)
        (run-parser table start-state top-cat next-input parse-error)))))

(defun run-parser (table start-state top-cat next-input parse-error)
  "Interpreter loop equivalent to the LALR-PARSER produced by the
   original generator. STATE-STACK and VAL-STACK shadow each other."
  (let ((cat-la '())
        (val-la '())
        (val-stack '())
        (state-stack '())
        (state start-state))
    (labels ((peek ()
               (unless cat-la
                 (let ((new (funcall next-input)))
                   (setf cat-la (list (car new))
                         val-la (list (cdr new)))))
               (first cat-la))
             (do-shift (where)
               (push state state-stack)
               (pop cat-la)
               (push (pop val-la) val-stack)
               (setf state where))
             (do-reduce (mother npop action)
               (if (eq mother top-cat)
                   (return-from run-parser (pop val-stack))
                   (let ((daughters '())
                         (st state))
                     (dotimes (_ npop)
                       (push (pop val-stack) daughters)
                       (setf st (pop state-stack)))
                     (push mother cat-la)
                     (push (apply action daughters) val-la)
                     (setf state st)))))
      (loop
        (let* ((entry (gethash state table))
               (cat (peek))
               (shift (assoc cat (getf entry :shifts))))
          (cond
            (shift (do-shift (cdr shift)))
            (t
             (let ((red (find-if (lambda (r) (member cat (first r)))
                                 (getf entry :reduces))))
               (if red
                   (destructuring-bind (la-list pos action mother) red
                     (declare (ignore la-list))
                     (do-reduce mother pos action))
                   (return (funcall parse-error)))))))))))

(defun run (parser tokens)
  "Convenience: run PARSER against a list of (cat . val) cons cells."
  (let ((toks tokens))
    (funcall parser
             (lambda () (or (pop toks) (cons :eof nil)))
             (lambda () (error "lalr: parse error before ~S" toks)))))

;;; *EOF*
