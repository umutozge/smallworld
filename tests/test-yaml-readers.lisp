;;; Unit test for the safe YAML readers in code/lexicon-reader.lisp
;;; (intern-yaml-token and safe-read-sexp-from-string).

;; lexicon-reader pulls in cl-yaml/uiop/etc that aren't on the test
;; sandbox.  We don't need read-lexicon for this test -- only the two
;; helpers, which are at the top of the file -- so we eval them here in
;; isolation.

;; Run in CL-USER (the same package lexicon-reader is loaded into in
;; production) so that interning symbols matches what read-from-string
;; would produce for the same input.
(in-package #:cl-user)

(defun intern-yaml-token (str)
  (check-type str string)
  (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) str))))

(defun safe-read-sexp-from-string (str)
  (check-type str string)
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable nil)))
    (with-input-from-string (in str)
      (let* ((eof (load-time-value (cons nil nil)))
             (form (read in nil eof))
             (rest (read in nil eof)))
        (when (eq form eof)
          (error "safe-read-sexp-from-string: empty input ~S" str))
        (unless (eq rest eof)
          (error "safe-read-sexp-from-string: trailing data after ~S in ~S"
                 form str))
        form))))

(defparameter *failures* 0)

(defun check (name expected actual)
  (if (equal expected actual)
      (format t "  ok    ~A~%" name)
      (progn
        (incf *failures*)
        (format t "  FAIL  ~A~%        expected: ~S~%        actual:   ~S~%"
                name expected actual))))

(format t "~%intern-yaml-token~%")

(check "interns 'dog' as DOG"
       'dog
       (intern-yaml-token "dog"))
(check "trims surrounding whitespace"
       'sg
       (intern-yaml-token "  sg  "))
(check "matches read-from-string for plain identifier"
       (read-from-string "noun")
       (intern-yaml-token "noun"))

;; Critical safety property: input that would have triggered read-eval
;; under read-from-string is now treated as a literal symbol.
(check "treats read-eval payload as a literal symbol"
       t
       (symbolp (intern-yaml-token "#.(error \"pwned\")")))

(format t "~%safe-read-sexp-from-string~%")

(check "reads a flat list"
       '(agr pl sg)
       (safe-read-sexp-from-string "(agr pl sg)"))
(check "reads a nested list"
       '(np (cat n) (bar 2))
       (safe-read-sexp-from-string "(np (cat n) (bar 2))"))
(check "matches read-from-string for valid sexpr"
       (read-from-string "(np (cat n) (bar 2))")
       (safe-read-sexp-from-string "(np (cat n) (bar 2))"))
(check "rejects #. read-time eval"
       :rejected
       (handler-case (safe-read-sexp-from-string "#.(error \"pwned\")")
         (error () :rejected)))
(check "rejects trailing junk"
       :rejected
       (handler-case (safe-read-sexp-from-string "(a b) c")
         (error () :rejected)))

(format t "~%~%~A failure(s).~%" *failures*)
(if (zerop *failures*) (sb-ext:exit :code 0) (sb-ext:exit :code 1))
