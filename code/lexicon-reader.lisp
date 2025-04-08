;;;
;;; Module for parsing CCG syn-grammars into lexicons
;;;

;;;
;;;

(defmacro lalr-parse (words with parser-package-name)
  `(let ((new-words (append ,words (list '$))))
    (labels ((lookup (word)
               (cadr (assoc word ,(intern (concatenate 'string (subseq (symbol-name parser-package-name) 0 4) "LEXICON")))))
             (next-input ()
               (let* ((word (pop new-words))
                      (cat (lookup word)))
                 (cons cat                  ; category
                       (list cat word))))   ; value
             (parse-error ()
               (format nil "Error before ~a" new-words)))
      (,(intern "LALR-PARSER" parser-package-name)
         #'next-input #'parse-error))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;   Sem Parsing   ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sem-translator (token)
  (case (cadr token)
    (an 'and)
    (or 'or)
    (cn 'cond)
    (ex 'exists)
    (un 'forall)
    (lm 'lam)
    (ng 'neg)
    (eq 'equal)
    ))


(defparameter sem-lexicon '((ob ob)
                            (cb cb)
                            (op op)
                            (cp cp)
                            (dt dt)
                            (lm opr)
                            (ex opr)
                            (un opr)
                            (an bcon)
                            (or bcon)
                            (cn bcon)
                            (eq bcon)
                            (ng ucon)
                            (pr prime)
                            ($ $)))

(defun sem-tokenizer (sem)
  "tokenize the semantics given as string"
  (let ((store nil)
        (word-buffer (make-string 100))
        (pos 0))
    (do ((index 0 (+ index 1)))
        ((= index (length sem))
         (remove-if #'null
                    (reverse
                      (if (zerop pos)
                          store
                          (cons
                            (let* ((word (string-upcase (subseq word-buffer 0 pos)))
                                   (symb (intern word)))
                                  (push (list symb (if (= (length word) 1) 'var 'name)) sem-lexicon)
                                  symb)
                            store)))))
        (let ((current-char (char sem index)))
          (if (or (and (alpha-char-p current-char) (lower-case-p current-char)) (digit-char-p current-char))
              (progn
                (setf (aref word-buffer pos) current-char)
                (incf pos))
              (progn
                (if (not (zerop pos))
                    (let* ((word (string-upcase (subseq word-buffer 0 pos)))
                           (int? (parse-integer word :junk-allowed t)))
                      (push
                        (or int?
                            (let ((symb (intern word)))
                                  (push (list symb (if (= (length word) 1) 'var 'name)) sem-lexicon)
                              symb))
                        store)
                      (setf pos 0)))
                (push
                  (case current-char
                    (#\( 'OP)
                    (#\) 'CP)
                    (#\[ 'OB)
                    (#\] 'CB)
                    (#\\ 'LM)
                    (#\V 'OR)
                    (#\& 'AN)
                    (#\> 'CN)
                    (#\~ 'NG)
                    (#\. 'DT)
                    (#\= 'EQ)
                    (#\E 'EX)
                    (#\A 'UN)
                    (#\' 'PR)
                    )
                  store)))))))



(defparameter sem-grammar
        '((for --> pred                 #'(lambda (pr) pr))
          (for --> binder dt for        #'(lambda (b d f) (append b (list f))))
          (for --> binder for           #'(lambda (b f) (append b (list f))))
          (for --> op for cp            #'(lambda (op for cp) for))
          (binder --> opr var           #'(lambda (o v) (list (sem-translator o) (cadr v))))
          (for -->  for bcon for        #'(lambda (f1 con f2) (list (list (sem-translator con) f1) f2)))
          (for -->  ucon for            #'(lambda (ucon f) (list (sem-translator ucon) f)))
          (for -->  op for bcon for cp  #'(lambda (op f1 con f2 cp) (list (list (sem-translator con) f1) f2)))
          (pred --> pred _pred          #'(lambda (pr tr) (list pr tr)))
          (pred --> _pred               #'(lambda (tr) tr))
          (_pred --> term               #'(lambda (tr) tr))
          (_pred --> for                #'(lambda (for) for))
          (_pred --> op pred _pred cp   #'(lambda (op pr tr cp) (list pr tr)))
          (term --> const               #'(lambda (cn) (cadr cn)))
          (term --> var                 #'(lambda (vr) (cadr vr)))
          (const --> name prime         #'(lambda (nm p) nm))
          )
        )

(defparameter sem-lexforms
  '(ob cb op cp eq dt opr bcon ucon prime name var))

(eval (sem-parser:make-parser sem-grammar sem-lexforms '$))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Syn Parsing       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-lex:define-string-lexer syn-lex
  ("\\("       (return (values 'op $@)))
  ("\\)"       (return (values 'cp $@)))
  ("\\["       (return (values 'ob $@)))
  ("\\]"       (return (values 'cb $@)))
  ("\\\\"      (return (values 'bs $@)))
  ("\\d"       (return (values (parse-integer $@) $@)))
  ("\\w+"      (return (values (intern (string-upcase $@)) $@)))
  ("[+-]"      (return (values (intern (string-upcase $@)) $@)))
  ("="         (return (values 'eq $@)))
  ("/"         (return (values 'fs $@)))
  ("[*]"       (return (values 'sm $@)))
  ("[\\^]"       (return (values 'hm $@)))
  ("[%]"       (return (values 'xm $@)))
  ("\\?[xyz]+" (return (values (intern (string-upcase $@)) $@)))
  )

(defun syn-lexer (input)
 (let ((lexer (syn-lex input))
       (store nil))
   (do ((token (multiple-value-list (funcall lexer)) (multiple-value-list (funcall lexer)))) 
       ((null (car token)) (reverse (mapcar #'car store)))
       (push token store))))

(defparameter syn-lexicon '((ob ob)
                            (cb cb)
                            (op op)
                            (cp cp)
                            (eq eq)
                            (fs slash)
                            (bs slash)
                            (sm sm)
                            (xm sm)
                            (hm sm)
                            (?x var)
                            (?y var)
                            (?z var)
                            ($ $)
                            ))


(defparameter syn-grammar
        '(
          (expr --> expr oper term	  #'(lambda (cat1 oper cat2) (list (list 'in cat2) oper (list 'out cat1))))
          (expr --> term	          #'(lambda (cat) cat))
          (term --> acat fstr 		  #'(lambda (acat fstr) (expand-base-category (cons (cadr acat) fstr))))
          (term --> op expr cp 		  #'(lambda (op cat cp) cat))
          (oper --> slash mode            #'(lambda (slash mode) (list 'slash (list (list 'dir (expand-slash slash)) mode))))
          (mode --> sm                    #'(lambda (mode) (list 'mode (expand-mode mode))))
          (mode -->                       #'(lambda () (list 'mode 'dot)))
          (fstr --> ob feat f cb          #'(lambda (ob feat f cb) (cons feat f)))
          (fstr -->         		  #'(lambda () nil))
          (f --> feat f       	          #'(lambda (feat f) (cons feat f)))
          (f -->          		  #'(lambda () nil))
          (feat --> fname eq feat         #'(lambda (fname eq feat) (list (cadr fname) (cadr feat))))
          (feat --> var      	          #'(lambda (var) var))
          (feat --> fval      	          #'(lambda (fval) (list 'fabv (cadr fval))))
          )
        )


(defparameter syn-lexforms
  '(acat ob cb op cp slash sm eq fname fval))

(eval (syn-parser:make-parser syn-grammar syn-lexforms '$))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Category expansion    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"This section works on the outputs of the lalr-parser, turning them into lexical categories and writing them to the debug lexicon file. That file will be read by uni-cg module to construct the online lexicon for parsing linguistic input."

(defun expand-slash (slash)
  (case (cadr slash)
    (fs 'forward)
    (bs 'backward)))

(defun expand-mode (mode)
  (case (cadr mode)
    (sm 'star)
    (xm 'cross)
    (hm 'harmonic)
    ))

(defun expand-base-category (cat)
  (labels ((feature-abrv-p (feat)
             (equal (car feat) 'fabv))

           (variable-p (sym)
             (char= #\? (aref (symbol-name sym) 0)))

           (feature-canceller-p (feat)
             (equal (cadr feat) 'fcancel))

           (update-fs (fs keyval)
             (let ((feature (car keyval))
                   (value (cadr keyval)))
               (cond ((endp fs) (list keyval))
                     ((equal feature (caar fs)) (if (not (variable-p (cadar fs)))
                                                    (error (make-condition 'default-feature-override
                                                                           :default feature
                                                                           :overrider value))
                                                    (cons keyval (cdr fs))))
                     (t (cons (car fs) (update-fs (cdr fs) keyval))))))

           (lookup-feature-name (fvalue)
             (let ((val  (rassoc fvalue (*state* :feature-dictionary) :test #'member)))
               (if val
                   (car val)
                   (error (make-condition 'invalid-feature-value :feature 'abbrv :value fvalue)))))

           (valid-value? (fname fvalue)
             (or
               (member fvalue (assoc
                                fname
                                (*state*
                                  :feature-dictionary)))
               (handler-case
                 (char= #\? (aref (symbol-name fvalue) 0))
                 (type-error (e)
                             (values nil e)))))

           (expand-feature (feat)
             (cond ((feature-abrv-p feat)
                    (let ((feat (cadr feat)))
                      (list (lookup-feature-name feat) feat)))
                   (t (let* ((name (car feat))
                             (value (cadr feat)))
                        (if (valid-value? name value)
                            feat
                            (error
                              (make-condition 'invalid-feature-value
                                              :feature name
                                              :value value)))))))

           (expand-cat (defined-features default-features)
             (reduce
               #'update-fs
               defined-features
               :initial-value default-features
               )))

    (let ((default-features (unifier:refresh-vars (cdr (assoc (car cat) (*state* :category-bundles)))))
          (defined-features (cdr cat)))
      (expand-cat (mapcar
                    #'expand-feature
                    defined-features)
                  (expand-cat default-features (unifier:refresh-vars (*state* :category-template)))
                  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Main drive       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                       (let ((syn-cat (lalr-parse (syn-lexer syn) with :syn-parser)))
                         (if (typep syn-cat 'string)
                             (error (make-condition 'bad-syntactic-type
                                                    :definition syn))
                             syn-cat))
                       (let ((sem-interp (lalr-parse (sem-tokenizer sem) with :sem-parser)))
                         (if (typep sem-interp 'string)
                             (error (make-condition 'bad-semantic-interpretation
                                                    :definition sem))
                             sem-interp))
                       phons)))
               entries)))

    (let* ((project-data (cl-yaml:parse (uiop:read-file-string "basic.yaml")))
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
