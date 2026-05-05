;;;
;;; The semantic-interpretation grammar.
;;;
;;; This module defines:
;;;
;;;   * SEM-TRANSLATOR -- maps lexical-form symbols (an, or, cn, ...)
;;;     to the logical-form connective names used in lambda terms.
;;;   * SEM-LEXICON -- the static token-class alist used to drive the
;;;     LALR parser.  Lower-case identifiers found in a particular
;;;     semantics string (constants and variables) are NOT pushed into
;;;     this global; SEM-TOKENIZER returns them as a per-call overlay
;;;     instead, which keeps each parse self-contained.
;;;   * SEM-TOKENIZER -- hand-rolled tokenizer for semantic strings
;;;     like `\x. dog x`.  Returns two values: the token list and an
;;;     overlay alist of `(symbol class)` entries (class is VAR for
;;;     single-letter identifiers, NAME otherwise) for the caller to
;;;     merge into the lexicon for this one parse.
;;;   * SEM-GRAMMAR -- the CFG for semantic interpretations, with
;;;     reduce actions that build the lambda-term internal
;;;     representation.
;;;   * *SEM-PARSER* -- a parser closure produced by LALR:MAKE-PARSER.
;;;

(defun sem-translator (token)
  (case (cadr token)
    (an 'and)
    (or 'or)
    (cn 'cond)
    (ex 'exists)
    (un 'forall)
    (lm 'lam)
    (ng 'neg)
    (lt 'prec)
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
                            (lt bcon)
                            (ng ucon)
                            (pr prime)
                            ($ $)))

(defun sem-tokenizer (sem)
  "Tokenize the semantics given as string.

   Returns two values:
     1. the list of tokens (symbols and integers, end-marker not appended);
     2. an alist overlay of identifier classes -- entries of the form
        (SYMBOL VAR) for single-letter identifiers and (SYMBOL NAME) for
        longer ones -- that the caller merges with SEM-LEXICON for this
        one parse.  No global state is mutated."
  (let ((store nil)
        (overlay nil)
        (word-buffer (make-string 100))
        (pos 0))
    (labels ((flush-word ()
               (when (plusp pos)
                 (let* ((word (string-upcase (subseq word-buffer 0 pos)))
                        (int? (parse-integer word :junk-allowed t)))
                   (push
                     (or int?
                         (let ((symb (intern word)))
                           (push (list symb (if (= (length word) 1) 'var 'name)) overlay)
                           symb))
                     store)
                   (setf pos 0)))))
      (do ((index 0 (+ index 1)))
          ((= index (length sem))
           (flush-word)
           (values (remove-if #'null (reverse store)) overlay))
        (let ((current-char (char sem index)))
          (if (or (and (alpha-char-p current-char) (lower-case-p current-char))
                  (digit-char-p current-char))
              (progn
                (setf (aref word-buffer pos) current-char)
                (incf pos))
              (progn
                (flush-word)
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
                    (#\< 'LT)
                    (#\E 'EX)
                    (#\A 'UN)
                    (#\' 'PR))
                  store))))))))

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
  '(ob cb op cp eq lt dt opr bcon ucon prime name var))

(defparameter *sem-parser* (lalr:make-parser sem-grammar sem-lexforms '$))
