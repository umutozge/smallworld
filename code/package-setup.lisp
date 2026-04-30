;;;
;;; package-setup.lisp -- runs first under smallworld.asd.
;;;
;;; The project's source files reference CL-PPCRE through its full
;;; package name; this file does NOT need to run for compilation to
;;; succeed.  What it does add are two convenience nicknames -- PPCRE
;;; and RE -- so that interactive REPL sessions can write
;;; `(re:regex-replace-all ...)` or `(ppcre:scan ...)` without having
;;; to spell out CL-PPCRE every time.
;;;
;;; The body is idempotent: loading this file twice (e.g. after a
;;; (asdf:load-system :smallworld :force t)) is a no-op once the
;;; nicknames exist, and it never errors if CL-PPCRE happens not to be
;;; loaded yet.
;;;

(when (find-package "CL-PPCRE")
  (dolist (nick '("PPCRE" "RE"))
    (unless (find-package nick)
      (rename-package "CL-PPCRE" "CL-PPCRE"
                      (cons nick (package-nicknames "CL-PPCRE"))))))
