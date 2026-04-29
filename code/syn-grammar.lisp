;;;
;;; The syntactic-category grammar.
;;;
;;; This module defines:
;;;
;;;   * SYN-LEX / SYN-LEXER -- a CL-LEX tokenizer for category strings
;;;     like `s\np[agr=sg]/np`.
;;;   * SYN-LEXICON -- the static token-class alist used to drive the
;;;     LALR parser.  At project load time, READ-LEXICON augments this
;;;     alist with feature names (as `fname`), feature values (as
;;;     `fval`), and category-bundle symbols (as `acat`) drawn from
;;;     the project's YAML.
;;;   * SYN-GRAMMAR -- the CFG for syntactic categories, with reduce
;;;     actions that build the AVM internal representation.
;;;   * EXPAND-SLASH / EXPAND-MODE -- helpers used only by SYN-GRAMMAR's
;;;     reduce actions.
;;;   * *SYN-PARSER* -- a parser closure produced by LALR:MAKE-PARSER.
;;;
;;; Reduce actions reference EXPAND-BASE-CATEGORY (defined in
;;; category-expand.lisp); since the lambdas are sharp-quoted forms in
;;; a quoted grammar list, the symbol resolves at call time, so load
;;; order between the two files is flexible.
;;;

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

(defparameter *syn-parser* (lalr:make-parser syn-grammar syn-lexforms '$))
