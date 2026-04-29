;;;
;;; Module for reading project YAML and producing the debug lexicon.
;;;
;;; This file is now a thin orchestration layer.  The pieces it used to
;;; carry inline have moved to dedicated modules:
;;;
;;;   * code/syn-grammar.lisp       -- syntactic-category lexer/grammar/parser
;;;   * code/sem-grammar.lisp       -- semantic-interpretation lexer/grammar/parser
;;;   * code/category-expand.lisp   -- EXPAND-BASE-CATEGORY (AVM completion)
;;;
;;; What stays here:
;;;
;;;   * LALR-PARSE-TOKENS  -- a small helper that drives a parser closure
;;;     (from LALR:MAKE-PARSER) over a token list and an alist lookup.
;;;   * READ-LEXICON       -- reads the project's YAML, populates the
;;;     project state (feature dictionary, category bundles, ...),
;;;     extends SYN-LEXICON with project-specific feature names/values
;;;     and category-bundle symbols, and writes the expanded debug
;;;     lexicon used downstream by the CCG module.
;;;

(defun lalr-parse-tokens (tokens parser lookup)
  "Drive PARSER (a closure from LALR:MAKE-PARSER) over the list of
   TOKENS, classifying each via LOOKUP (an alist of (token . category)).
   The trailing $ end-marker is appended automatically."
  (let ((words (append tokens (list '$))))
    (labels ((next-input ()
               (let* ((word (pop words))
                      (cat (cadr (assoc word lookup))))
                 (cons cat (list cat word))))
             (parse-error ()
               (format nil "Error before ~a" words)))
      (funcall parser #'next-input #'parse-error))))


(defun read-lexicon ()

  (labels ((lex-entry-to-alist (lex-ht)
             (let ((store nil))
               (maphash 
                 #'(lambda (k v) 
                     (push (list (read-from-string k) v) store))
                 lex-ht)
               store)) 

           (explode-syns (lex-ht)
             "syn may be a single category or a list. Explode the entry accordingly for multiple syns "
             (check-type lex-ht hash-table)
             (let ((syn (gethash "syn" lex-ht)))
               (typecase syn
                 (cons (mapcar #'(lambda (x)
                                   (let ((new-ht (alexandria:copy-hash-table lex-ht)))
                                     (setf (gethash "syn" new-ht) x)
                                     new-ht))
                               syn))
                 (t (list lex-ht)))))

           (fix-phon (lex-ht)
             "phons come as list of strings or a single string from yaml, this turns them into list of symbols"
             (check-type lex-ht hash-table)
             (let ((phon (gethash "phon" lex-ht)))
               (setf (gethash "phon" lex-ht)
                     (if (stringp phon)
                         (list (read-from-string phon))
                         (mapcar #'read-from-string phon)))
               lex-ht))

           (build-entry (pos syn sem phon)
             `((pos  ,pos)
               (syn  ,syn)
               (sem  ,(sublis (list (cons 'common-lisp-user::lex phon)) sem))
               (phon ,phon)))

           (generate-entry-list (entries)
             (mapcar
               #'(lambda (entry)
                   (destructuring-bind
                     (pos syn sem phons)
                     entry
                     (list
                       (read-from-string pos)
                       (let ((syn-cat (lalr-parse-tokens (syn-lexer syn) *syn-parser* syn-lexicon)))
                         (if (typep syn-cat 'string)
                             (error (make-condition 'bad-syntactic-type
                                                    :definition syn))
                             syn-cat))
                       (let ((sem-interp (lalr-parse-tokens (sem-tokenizer sem) *sem-parser* sem-lexicon)))
                         (if (typep sem-interp 'string)
                             (error (make-condition 'bad-semantic-interpretation
                                                    :definition sem))
                             sem-interp))
                       phons)))
               entries)))

    (let* ((project-data (handler-case (cl-yaml:parse (uiop:read-file-string (*state* :project)))
                           (YAML.ERROR:PARSING-ERROR (condition)
                                                    (error (make-condition 'bad-YAML
                                                                    :file-name (pathname-name (*state* :project))
                                                                    :message (YAML.ERROR:message condition)
                                                                    :column (YAML.ERROR:column condition)
                                                                    :line (YAML.ERROR:line condition))))))
           (feature-dictionary (mapcar #'read-from-string (gethash "feature-dictionary" project-data)))
           (category-bundles (mapcar #'read-from-string (gethash "category-bundles" project-data)))
           (lexicon (gethash "lexicon" project-data))
           (entries 
             (mapcar
               #'(lambda (entry)
                   (list (cadr (assoc 'pos entry))
                         (cadr (assoc 'syn entry))
                         (cadr (assoc 'sem entry))
                         (cadr (assoc 'phon entry))))
               (remove-if
                       #'(lambda (x)
                           (member (cadr (assoc 'status x)) '("off" "false" "inactive") :test #'string=))
                       (mapcan
                         ; map each entry to a list to handle expansions due to multiple syns and collect them in a list
                         #'(lambda (lex-ht)
                             (check-type lex-ht hash-table)
                             (mapcar #'lex-entry-to-alist 
                                     (explode-syns (fix-phon lex-ht))))
                         ; check if multiple entries to avoid error in a singleton lexicon
                         (if (consp lexicon) lexicon (list lexicon)))))
             ))

      "set state variables"
      (*state* :feature-dictionary feature-dictionary)
      (*state* :category-bundles category-bundles)
      (*state* :features                (mapcar #'car (*state* :feature-dictionary)))
      (*state* :category-template       (mapcar #'(lambda (x) (list x (gensym "?"))) (*state* :features)))

      "add items to the syn-lexicon on the basis of *feature-dictionary*"
      (dolist (x feature-dictionary)
        (dolist (y (cdr x))
          (push (list
                  (if (integerp y)
                      y;(intern (string (digit-char y)))
                      y)
                  'fval)
                syn-lexicon))
        (push (list (car x) 'fname) syn-lexicon))

      "add *category-bundle-symbols* as acat items to the syn-lexicon"
      (dolist (x (copy-alist category-bundles))
        (push (list (car x) 'acat) syn-lexicon))

      "now open the lex file and parse it"
      (let ((store nil))
        (with-open-file (debug-stream (*state* :debug-lexicon-path) :direction :output :if-exists :supersede)
          (dolist (entry (generate-entry-list entries))
            (destructuring-bind
              (pos syn sem phons)
              entry
              (dolist (phon phons)
                (let ((item (build-entry  pos syn sem phon)))
                  (format debug-stream "~A~%~%" item)
                  (push item store))))))
        store))))
