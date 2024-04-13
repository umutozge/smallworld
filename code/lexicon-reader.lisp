;;;
;;; Module for parsing CCG syn-grammars into lexicons
;;;

;;;
;;; Input format:
;;;
;;; def <cat-name> {s\np; \x.lex'x; dog cat}:   
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
    (ng 'neg)))


(defparameter sem-lexicon '((ob ob)
                            (cb cb)
                            (op op)
                            (cp cp)
                            (eq eq)
                            (dt dt)
                            (lm opr)
                            (ex opr)
                            (un opr)
                            (an bcon)
                            (or bcon)
                            (cn bcon)
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

(defun syn-tokenizer (cat)
  "tokenize the category given as string"
  (let ((store nil)
        (word-buffer (make-string 100))
        (pos 0))
    (do ((index 0 (+ index 1)))
        ((= index (length cat))
         (remove-if #'null
                    (reverse
                      (if (zerop pos)
                          store
                          (cons (intern (string-upcase (subseq word-buffer 0 pos))) store)))))
        (let ((current-char (char cat index)))
          (if (or (alpha-char-p current-char) (digit-char-p current-char))
              (progn
                (setf (aref word-buffer pos) current-char)
                (incf pos))
              (progn
                (if (not (zerop pos))
                    (let* ((word (string-upcase (subseq word-buffer 0 pos)))
                           (int? (parse-integer word :junk-allowed t)))
                      (push
                        (or int?
                            (intern word))
                        store)
                      (setf pos 0)))
                (push
                  (case current-char
                    (#\( 'OP)
                    (#\) 'CP)
                    (#\[ 'OB)
                    (#\] 'CB)
                    (#\/ 'FS)
                    (#\\ 'BS)
                    (#\= 'EQ)
                    (#\* 'SM) ;slash modality
                    (#\! 'FC) ;feature canceller
                    (#\+ '+)
                    (#\- '-))
                  store)))))))


(defparameter syn-lexicon '((ob ob)
                            (cb cb)
                            (op op)
                            (cp cp)
                            (eq eq)
                            (fs slash)
                            (bs slash)
                            (sm sm)
                            (fc fc)
                            ($ $)
                            ))


(defparameter syn-grammar
        '((cat --> acat fstr 		  #'(lambda (acat fs) (expand-base-category (cons (cadr acat) fs))))
          (cat --> cat oper cat	          #'(lambda (cat1 oper cat2) (list (list 'in cat2) oper (list 'out cat1))))
          (cat --> op cat cp 		  #'(lambda (op cat cp) cat))
          (oper --> slash mode            #'(lambda (slash mode) (list 'slash (list (list 'dir (expand-slash slash)) mode))))
          (mode --> sm                    #'(lambda (mode) (list 'mode (expand-mode mode))))
          (mode -->                       #'(lambda () (list 'mode 'dot)))
          (fstr --> ob feat f cb          #'(lambda (ob feat f cb) (cons feat f)))
          (fstr -->         		  #'(lambda () nil))
          (f --> feat f       	          #'(lambda (feat f) (cons feat f)))
          (f -->          		  #'(lambda () nil))
          (feat --> fname eq feat         #'(lambda (fname eq feat) (list (cadr fname) (cadr feat))))
          (feat --> fc fname              #'(lambda (fc fname) (list (cadr fname) 'fcancel)))
          (feat --> fval      	          #'(lambda (fval) (list 'fabv (cadr fval))))) 
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
    (sm 'star)))

(defun expand-base-category (cat)
  (labels ((feature-abrv-p (feat)
             (equal (car feat) 'fabv))

           (feature-canceller-p (feat)
             (equal (cadr feat) 'fcancel)
             )

           (lookup-feature-name (fvalue)
             (let ((val  (rassoc fvalue (*state* :feature-dictionary) :test #'member)))
               (if val
                   (car val)
                   (error (format nil "ERROR: Feature value ~a is unknown." fvalue)))))

           (expand-feature (feat)
             (cond ((feature-abrv-p feat)
                    (let ((feat (cadr feat)))
                      (list (lookup-feature-name feat) feat)))
                   ((feature-canceller-p feat)
                    (list (car feat) (gensym "?")))
                   (t (let* ((name (car feat))
                             (value (cadr feat))
                             (value-valid? (member value (assoc
                                                           name
                                                           (*state*
                                                             :feature-dictionary)))))
                        (if value-valid?
                            feat
                            (error (format nil "ERROR: ~a is an invalid value for ~a." value name)))))))

           (expand-cat (cat-features overriding-features)
             (if (endp overriding-features)
                 cat-features
                 (expand-cat
                   (fs-update cat-features (car overriding-features))
                   (cdr overriding-features)))))

    (let ((cat-features (cdr (assoc (car cat) (*state* :category-bundles))))
          (overriding-features (cdr cat)))
      (expand-cat cat-features (mapcar
                                 #'expand-feature
                                 overriding-features)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Main drive       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-lexicon ()
  (labels ((read-entries (content)
             (let ((outer-scanner (re:create-scanner "def\\s+[^{]+\\s*{[^}]+}"))
                   (inner-scanner (re:create-scanner "def\\s+([^{( ]+)\\s*[(]([^)]*)[)][ \\t\\n]*{[ \\t\\n]*(.+)[ \\t\\n]*;[ \\t\\n]*([^}]+)[ \\t\\n]*;[ \\t\\n]*([^}]*)}")))
               (mapcar
                 #'(lambda (entry)
                     (re:register-groups-bind
                       (key cat syn sem forms)
                       (inner-scanner entry)
                       (list
                         (read-from-string key)
                         (if (or (string= cat "") (not (*state* :morphology)))
                             '_
                             (read-from-string cat))
                         syn
                         sem
                         forms)))
                 (let ((store nil))
                   (re:do-matches-as-strings
                     (match outer-scanner content store)
                     (push match store))))))
           (build-entry (key cat phon syn sem)
             `((key  ,key)
               (cat  ,cat)
               (phon ,phon)
               (syn  ,syn)
               (sem  ,(sublis (list (cons 'common-lisp-user::lex phon)) sem))))
           (generate-entry-list ()
             (mapcar
               #'(lambda (entry)
                   (destructuring-bind
                     (key cat syn sem tokens)
                     entry 
                     (list
                       key
                       (if (*state* :morphology) cat '_)
                       (lalr-parse (syn-tokenizer syn) with :syn-parser)
                       (lalr-parse (sem-tokenizer sem) with :sem-parser)
                       (aux:string-to-list tokens))
                       ))
               (read-entries (aux:read-file-as-string (*state* :lexicon-path))))))
    "then add items to the syn-lexicon on the basis of *feature-dictionary*"
    (dolist (x (*state* :feature-dictionary))
      (dolist (y (cdr x))
        (push (list y 'fval) syn-lexicon))
      (push (list (car x) 'fname) syn-lexicon))
    "then add *category-bundle-symbols* as acat items to the syn-lexicon"
    (dolist (x (copy-alist (*state* :category-bundles)))
      (push (list (car x) 'acat) syn-lexicon))
    (let ((store nil))
      (with-open-file (debug-stream (*state* :debug-lexicon-path) :direction :output)
        (dolist (entry (generate-entry-list))
          (destructuring-bind
            (key cat syn sem tokens)
            entry
            (dolist (token tokens)
              (let ((item (build-entry key cat token syn sem)))
                (format debug-stream "~A~%~%" item)
                (push item store))))))
      store)))
