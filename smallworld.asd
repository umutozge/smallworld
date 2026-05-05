;;;; smallworld.asd
;;;;
;;;; ASDF system definition for SmallWorld -- a CCG-based natural-language
;;;; parser.
;;;;
;;;; Loading the system:
;;;;
;;;;   (ql:quickload :smallworld)             ; if SmallWorld is on the
;;;;                                          ; ASDF central registry
;;;;
;;;;   ;; or, when running from a fresh checkout:
;;;;   (push (truename (uiop:getcwd)) asdf:*central-registry*)
;;;;   (asdf:load-system :smallworld)
;;;;
;;;; Components are listed in load order: a file may rely on names
;;;; defined in any earlier file.  See code/main.lisp for the runtime
;;;; entry points (MAIN, PROC-INPUT, etc.) once the system is loaded.

(defsystem "smallworld"
  :description "A CCG-based natural-language parser."
  :author      "Umut Ozge <umozge@gmail.com>"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ("uiop"
                "str"
                "cl-ppcre"
                "unix-opts"
                "cl-lex"
                "cl-yaml"
                "alexandria")
  :pathname    "code/"
  :serial      t
  :components  ((:file "package-setup")
                (:file "service")
                (:file "conditions")
                (:file "utils")
                (:file "unifier")
                (:file "lc-q")
                (:file "sr-parser")
                (:file "lalr")
                (:file "category-expand")
                (:file "syn-grammar")
                (:file "sem-grammar")
                (:file "lexicon-reader")
                (:file "ccg")
                (:file "morphology")
                (:file "command-line")
                (:file "main")))
