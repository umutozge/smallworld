; (declaim (optimize (speed 0) (safety 3) (debug 3)))
;(declaim (sb-ext:muffle-conditions cl:warning))
(sb-ext:enable-debugger)

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :uiop :silent t)
(ql:quickload :str :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :unix-opts :silent t)
(ql:quickload :cl-lex :silent t)
(ql:quickload :cl-yaml :silent t)
(ql:quickload :alexandria :silent t)
(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

;; load the components
(mapc
  (lambda (name)
    (load (str:concat name ".lisp")))
  '("service" "conditions" "utils" "unifier" "lc-q" "sr-parser" "syn-parser" "sem-parser" "lexicon-reader" "ccg" "morphology" "command-line"))



(setf *print-pretty* t)

;; init a global *state* closure available everywhere
(setf  (symbol-function '*state*)
       (let ((table (make-hash-table)))
         #'(lambda (key &optional (value '<NULL>))
             (if (not (equal value '<NULL>) )
                 (setf (gethash key table) value)
                 (multiple-value-bind (val status)
                   (gethash key table)
                   val)))))


;; set global switches
(*state* :eta nil)
(*state* :verbose nil)
(*state* :uniq t)


(defun proc-input (input)
  "Handle the console input"
  (case (car input)
    ((:debug :db)
     (break)
     (return-from proc-input nil))
    ((nil) (return-from proc-input nil))
    ((:help :h)
     (display-help))
    ((:quit :q) (princ "bye, come again!") (terpri) (quit))
    ((:list-vocab :l)
     (format t "~{~{~10A~^ ~}~%~}" (aux:partition (mapcar
                                                    #'(lambda (x)
                                                        (format nil "~a (~a)" (first x) (second x)))
                                                    (sort
                                                      (mapcar
                                                        #'(lambda (lexkey)
                                                            (list (lexkey-phon lexkey) (lexkey-pos lexkey)))
                                                        (*state* 'vocab))
                                                      #'string<
                                                      :key #'(lambda (x) (symbol-name (second x)))
                                                      ))
                                                  8))
     (terpri))
    ((:eta :e)
     (princ (toggle-flag :eta)) (terpri))
    ((:verbose :v)
     (princ (toggle-flag :verbose)) (terpri))
    ((:uniq :u)
     (princ (toggle-flag :uniq)) (terpri))
    ((:parse :p)
       (let ((expression (cdr input))) (display-parses expression (parse-expression expression))) (terpri))
    ((:parse-file :pf)
      (parse-file (cadr input)) (terpri))
    ((:reload :r) (main))
    (read-error (princ "unknown command") (terpri))
    (otherwise (cond ((keywordp (car input)) (princ "unknown command") (terpri))
                     (t (display-parses input (parse-expression input)) (terpri))))))

(defun display-help ()
  (let ((data '((":parse (:p) <expression>" "parse the provided expression")
                (":list-vocab (:l)" "display the vocabulary")
                (":eta (:e)" "turn on/off eta-normalization of logical forms")
                (":uniq (:u)" "turn on/off eliminating semantically spurious parses")
                (":verbose (:v)" "verbose output for inspection")
                (":reload (:r)" "reload the project")
                (":help (:h)" "help")
                (":quit (:q)" "quit"))))
  (format t "~%")
  (format t "~{~{~24A~^ -- ~} ~%~}" data)
  (format t "~%")))

(defun parse-file (filename)
  (let* ((inpath (merge-pathnames (string-downcase filename)))
         (outpath (make-pathname :name (pathname-name inpath) :directory (pathname-directory inpath) :type "out"))
         (sentences (handler-case (with-open-file (str inpath :direction :input :if-does-not-exist :error)
                                    (reverse
                                      (remove-if #'null
                                                 (mapcar
                                                   #'(lambda (string)
                                                       (read-from-string (str:concat "(" string ")")))
                                                   (do  ((sentence (read-line str nil nil) (read-line str nil nil))
                                                         (store nil (cons sentence store)))
                                                        ((null sentence) store))))))
                      (FILE-DOES-NOT-EXIST (err)
                                           (format t "~%The file ~A cannot be found.~%~%Check path and make sure to avoid capital letters and spaces in your filenames.~%" (namestring inpath))
                                           (return-from parse-file 0)
                                           ))))

    (if (probe-file outpath) (delete-file outpath))

    (with-open-file (str outpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (format t "Parsing ~A.~% Abort with Ctrl-C.~%" outpath)
      (dolist (i sentences)
        (display-parses i (parse-expression i) str))
      (format t "~%Output written to ~A.~%" (namestring outpath))
      )))

(defun display-parses (input-expression parses &optional (str t))
  (if parses
      (mapc
        #'(lambda (item)
            (typecase (cadr item)
              (string (format str "~%~A~%" (cadr item))) ; ERROR 
              (list (let ((index (car item))
                          (result (caadr item))
                          (derivation (cadr item)))

                      (format str "~%PARSED: ~{~A ~}~%" input-expression)
                      (format str "~%--------------------PARSE ~D--------------------~%" index)
                      (if (*state* :verbose) (format t "~A~%" (caadr item)))
                      (format str "~%~A~%" (pretty-print :type :sign :format :text :form result))
                      (format str "~%-----------------------------------------------~%")
                      (when (*state* :verbose)
                        (format str "~%--------------------DERIV ~D--------------------~%" (car item))
                        (aux:print-binary-tree (aux:maptree #'(lambda (x) (pretty-print :type :sign :format :text :form x)) derivation) 0 2 str)
                        (format str "~%------------------------------------------------~%"))))))
        parses
        )

        (format str "~%*~{~A~^ ~}~%" input-expression) ; FAILURE

      ))

(defun parse-expression (expression)
  "return a numeration of parse
   ((index result derivation)...(index result derivation))"
  (aux:enum
    (funcall
      (if (*state* :uniq)
          #'uniq-parses
          #'identity)
      (handler-case (parse expression)
        (NO-MORPH-PARSE (e)
                        (list (format nil "No morph parse for ~A." (input e))))
        (ITEM-NOT-FOUND (e)
                        (list (format nil "~A (~A) is not in your lexicon." (lexkey-phon (lexkey e)) (lexkey-pos (lexkey e)))))))
    1))

(defun toggle-flag (flag)
  (format nil "~A is ~A" flag (let ((state (*state* flag (not (*state* flag)))))
                                (if state 'on 'off))))

(defun command-line ()
  (or
    #+CLISP *args*
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    nil))

(defun main ()

  (run-program "/usr/bin/clear" nil :output *standard-output*)
  (handler-case (parse-command-line)
    (TYPE-ERROR (condition)
                (format t "~%~%~%Cannot find a project~%Run samllworld -h for help.~%")
                (sb-ext:quit)))
  (*state* :lexicon (aux:multiset-table))
  (handler-case (uiop:chdir (*state* :project-dir))
    (SB-POSIX:SYSCALL-ERROR (err)
                            (format t "~%Cannot find ~A~%~%" (*state* :project))
                            (sb-ext:quit)
                            )
    )

  (*state* :prompt (pathname-name (*state* :project)))
  (*state* :debug-lexicon-path (make-pathname :name "_lexicon" :type "lisp" :directory (pathname-directory (*state* :project-dir))))
  (if (probe-file (*state* :debug-lexicon-path))
      (delete-file (*state* :debug-lexicon-path)))
  (*state* :mode-table (aux:list-to-hash-table
                         '((star (star))
                           (harmonic (harmonic star))
                           (cross (cross star))
                           (dot (harmonic cross star)))))
  (format t "Welcome to SmallWorld~%~%A linguists' parser based on CCG~%~%Type :help for help, :quit for quit.~%")
  (format t "~%------------------------------" )
  (format t "~%Project: ~a" (pathname-name (*state* :project)))
  (format t "~%Loaded ~D items." (handler-case (load-lexicon)
                                   (BAD-YAML (condition)
                                             (format t "~%~%~%ERROR in ~A.yaml, Column ~A, Line ~A, ~A~%~%Revise and reload.~%~%"
                                                     (file-name condition)
                                                     (column condition)
                                                     (line condition)
                                                     (message condition))
                                             0)))
  (format t "~%------------------------------~%" )
  (*state* :morph-analyzer (handler-case (make-morph-analyzer)
                                                       (MISSING-MORPH-FILE (e)
                                                                           (format t "~%Cannot find an ~A.fst or ~A.mrf, aborting..~%~%" (file-name e) (file-name e))
                                                                           (sb-ext:quit))))
  (*state* 'vocab (funcall (*state* :lexicon) :keys))
  (format t "~%")
  (loop
    (format t "~a> " (*state* :prompt))
    (finish-output)
    (handler-case (proc-input (handler-case (read-from-string (str:concat "("(read-line) ")"))
                                (SB-INT:SIMPLE-READER-ERROR (e)
                                                            (list 'read-error))
                                (END-OF-FILE (e)
                                             (list 'read-error))))
      (SB-SYS:INTERACTIVE-INTERRUPT (e)
                                    (format t "~%User interrupt.~%~%")
                                    (sb-ext:quit)))))
