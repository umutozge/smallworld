;(declaim (optimize (speed 0) (safety 3) (debug 3)))
;(declaim (sb-ext:muffle-conditions cl:warning))

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :uiop :silent t)
(ql:quickload :str :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :unix-opts :silent t)
(ql:quickload :cl-lex :silent t)
(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command-line options ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(opts:define-opts
  (:name :morphology
   :description "invoke morphological parsing"
   :short #\m
   :long "morphology"))


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

(mapc
  (lambda (name)
    (load (str:concat name ".lisp")))
  '("service" "conditions" "utils" "unifier" "lc-q" "sr-parser" "syn-parser" "sem-parser" "lexicon-reader" "ccg" "flookup"))

(defun proc-input (input)
  (case (car input)
    ((:help :h)
     (display-help))
    ((:quit :q) (princ "bye, come again!") (terpri) (quit))
    ((:show-vocab :sv :ls)
     (format t "~{~{~10A~^ ~}~%~}" (aux:partition (mapcar
                                                    #'(lambda (x)
                                                        (format nil "~a (~a)" (first x) (second x)))
                                                    (sort
                                                      (mapcar
                                                        #'(lambda (lexkey)
                                                            (list (lexkey-phon lexkey) (lexkey-cat lexkey)))
                                                        (*state* 'vocab))
                                                      #'string<
                                                      :key #'(lambda (x) (symbol-name (second x)))
                                                      ))
                                                  8))
     (terpri))
    ((:switch-eta :se)
     (princ (toggle-flag :eta-normalization)) (terpri))
    ((:switch-debug-mode :sdm)
     (princ (toggle-flag :debug-mode)) (terpri))
    ((:switch-derivation :sd)
     (princ (toggle-flag :derivation)) (terpri))
    ((:switch-uniq :su)
     (princ (toggle-flag :uniq-parses)) (terpri))
    ((:parse :p)
       (let ((expression (cdr input))) (display-parses expression (parse-expression expression))) (terpri))
    ((:parse-file :pf)
      (parse-file (cadr input)) (terpri))
    ((:reload :rl) (main))
    (otherwise (cond ((keywordp (car input)) (princ "unknown command") (terpri))
                     (t (display-parses input (parse-expression input)) (terpri))))))

(defun display-help ()
  (let ((data '((":parse (:p) <expression>" "parse the provided expression")
                (":show-vocab (:sv)" "display the vocabulary")
                (":switch-derivation (:sd)" "turn on/off display of derivations")
                (":switch-eta (:se)" "turn on/off eta-normalization of logical forms")
                (":switch-uniq (:su)" "turn on/off eliminating semantically spurious parses")
                (":switch-debug-mode (:sdm)" "turn on/off debug messages")
                (":reload (:rl)" "reload the system")
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
                                      (mapcar
                                        #'(lambda (string)
                                            (read-from-string (str:concat "(" string ")")))
                                        (do  ((sentence (read-line str nil nil) (read-line str nil nil))
                                              (store nil (cons sentence store)))
                                             ((null sentence) store)))))
                      (FILE-DOES-NOT-EXIST (err)
                                           (format t "~%The file ~A cannot be found.~%~%Check path and make sure to avoid capital letters and spaces in your filenames.~%" (namestring inpath))
                                           (return-from parse-file 0)
                                           ))))

    (if (probe-file outpath) (delete-file outpath))

    (with-open-file (str outpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
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

                      (format str "~%--------------------PARSE ~D--------------------~%" index)
                      (if (*state* :debug-mode) (format t "~A~%" (caadr item)))
                      (format str "~%~A~%" (pretty-print :type :sign :format :text :form result))
                      (format str "~%------------------------------------------------~%")
                      (when (*state* :derivation)
                        (format str "~%--------------------DERIV ~D--------------------~%" (car item))
                        (format str "~A~%" (aux:maptree #'(lambda (x) (pretty-print :type :sign :format :text :form x)) derivation))
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
      (if (*state* :uniq-parses)
          #'uniq-parses
          #'identity)
      (handler-case (parse expression)
        (ITEM-NOT-FOUND (e)
                      (list (format nil "~A(~A) is not in your lexicon." (lexkey-phon (lexkey e)) (lexkey-cat (lexkey e)))))))
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

  (multiple-value-bind (options args)
    (opts:get-opts)
    (*state* :morphology (getf options :morphology))
    (*state* :project-path (merge-pathnames (let ((cl-pathname (if args
                                                                   (pathname (car args))
                                                                   (sb-posix:getcwd))))
                                                  (if (null (pathname-name cl-pathname))
                                                      cl-pathname 
                                                      (make-pathname
                                                       ; :defaults cl-pathname
                                                        :directory (append
                                                                     (or (pathname-directory cl-pathname)
                                                                         (list :relative))
                                                                     (list (pathname-name cl-pathname)))))))))
  (*state* :eta-normalize t)
  (*state* :debug-mode nil)
  (*state* :derivation nil)
  (*state* :uniq-parses nil)
  (*state* :lexicon (aux:multiset-table))

  (handler-case (uiop:chdir (*state* :project-path))
    (SB-POSIX:SYSCALL-ERROR (err)
                            (format t "~%Cannot find ~A~%~%" (*state* :project-path))
                            (sb-ext:quit)
                            )
    )

  (*state* :prompt (car (last (pathname-directory (*state* :project-path)))))

  (if (*state* :morphology) (setup-morph-analyzer (*state* :prompt)))

  (*state* :debug-lexicon-path (make-pathname :name "_lexicon" :type "lisp" :directory (pathname-directory (*state* :project-path))))
  (if (probe-file (*state* :debug-lexicon-path))
      (delete-file (*state* :debug-lexicon-path)))

  (*state* :lexicon-path (make-pathname :name (*state* :prompt) :type "lex" :directory  (pathname-directory (*state* :project-path))))
  (*state* :theory-path (make-pathname :name (*state* :prompt) :type "thr" :directory (pathname-directory (*state* :project-path))))
  (*state* :theory (aux:read-from-file (*state* :theory-path)))
  (*state* :feature-dictionary      (cdr  (assoc 'feature-dictionary (*state* :theory))))
  (*state* :features                (mapcar #'car (*state* :feature-dictionary)))
  (*state* :category-template       (mapcar #'(lambda (x) (list x (gensym "?"))) (*state* :features)))
  (*state* :category-bundles (cdr (assoc 'category-bundles (*state* :theory))))
  (run-program "/usr/bin/clear" nil :output *standard-output*)
  (format t "Welcome to SmallWorld~%~%A linguists' parser based on CCG~%~%Type :help for help, :quit for quit.~%")
  (format t "~%------------------------------" )
  (format t "~%Theory: ~a" (*state* :theory-path))
  (format t "~%Lexicon: ~a" (*state* :lexicon-path))
  (format t "~%Loaded ~D items." (load-lexicon))
  (format t "~%------------------------------~%" )
  (*state* 'vocab (funcall (*state* :lexicon) :keys))
  (format t "~%")
  (loop
    (format t "~a> " (*state* :prompt))
    (finish-output)
    (proc-input (handler-case (read-from-string (str:concat "("(read-line) ")"))
                  (sb-int:simple-reader-error (e)
                              'read-error)))))
