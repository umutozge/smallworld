;;; Unit test for sem-tokenizer.
;;;
;;; This test verifies that the tokenizer:
;;;   1. produces the right token sequence for representative inputs;
;;;   2. classifies single-letter identifiers as VAR and longer ones
;;;      as NAME via its overlay return-value;
;;;   3. does NOT mutate the global SEM-LEXICON between calls.

(load "code/lalr.lisp")
(load "code/sem-grammar.lisp")

(defparameter *failures* 0)

(defun check (name expected actual)
  (if (equal expected actual)
      (format t "  ok    ~A~%" name)
      (progn
        (incf *failures*)
        (format t "  FAIL  ~A~%        expected: ~S~%        actual:   ~S~%"
                name expected actual))))

(format t "~%sem-tokenizer~%")

;; Snapshot the lexicon before any tokenization.
(defparameter *lex-before* (copy-list sem-lexicon))

;; ---- input 1: a lambda term  "\x. dog x"
(multiple-value-bind (tokens overlay) (sem-tokenizer "\\x. dog x")
  (check "tokens for \\x. dog x"
         '(LM X DT DOG X)
         tokens)
  (check "overlay classifies single-letter X as VAR"
         t
         (and (member '(X VAR) overlay :test #'equal) t))
  (check "overlay classifies multi-char DOG as NAME"
         t
         (and (member '(DOG NAME) overlay :test #'equal) t)))

;; ---- input 2: "love j m"
(multiple-value-bind (tokens overlay) (sem-tokenizer "love j m")
  (check "tokens for love j m"
         '(LOVE J M)
         tokens)
  (check "LOVE is NAME, J and M are VAR"
         '(t t t)
         (list (and (member '(LOVE NAME) overlay :test #'equal) t)
               (and (member '(J VAR) overlay :test #'equal) t)
               (and (member '(M VAR) overlay :test #'equal) t))))

;; ---- input 3: integer literal
(multiple-value-bind (tokens overlay) (sem-tokenizer "5")
  (check "integer literal becomes integer token"
         '(5)
         tokens)
  (check "integer produces no overlay entry"
         nil
         overlay))

;; ---- mutation check: after several calls, sem-lexicon must be unchanged.
(check "sem-lexicon is not mutated by tokenization"
       *lex-before*
       sem-lexicon)

(format t "~%~%~A failure(s).~%" *failures*)
(if (zerop *failures*) (sb-ext:exit :code 0) (sb-ext:exit :code 1))
