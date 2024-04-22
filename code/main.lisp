(declaim (optimize (speed 0) (safety 3) (debug 3)))
;(declaim (sb-ext:muffle-conditions cl:warning))

(load "~/.sbclrc")
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
  '("aux" "utils" "unifier" "lc-q"  "sr-parser" "syn-parser" "sem-parser" "lexicon-reader" "ccg" "flookup"))

(defun proc-input (input)
  (case input
    ((:help :h)
     (display-help))
    ((:quit :q) (princ "bye, come again!") (terpri) (quit))
    ((:show-vocab :sv)
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
     (terpri) (princ (parse-expr)) (terpri) (terpri))
    ;((:parse-file :pf)
    ; (parse-file) (terpri))
    ((:reload :rl) (main))
    (otherwise (princ "unknown command") (terpri))))

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

;(defun parse-file (&optional fname)
;  (let* ((filename (or fname (read-line)))
;         (sentences (aux:tsv-to-list
;                      (make-pathname :name filename)))
;         (outpath (aux:string-to-pathname filename ".out")))
;    (with-open-file (str outpath :direction :output
;                         :if-does-not-exist :create
;                         :if-exists :overwrite)
;      (dolist (i sentences)
;        (format str (string-downcase "~{~A ~}~%~{~A~%~}~%~%")
;                i
;                (mapcar sign-sem (uniq-parses (parse-expr i)))))))) ; TODO can't display ambiguity

(defun parse-expr (&optional expr)
  (let* ((expression (or expr (aux:string-to-list (read-line)))))
    (progn
      (mapc
        #'(lambda (item)
            (let ((index (car item))
                  (result (caadr item))
                  (derivation (cadr item)))

              (format t "~%--------------------PARSE ~D--------------------~%" index)
              (if (*state* :debug-mode) (format t "~A~%" (caadr item)))
              (format t "~%~A~%" (pretty-print :type :sign :format :text :form result))
              (format t "~%------------------------------------------------~%")
              (when (*state* :derivation) 
                (format t "~%--------------------DERIV ~D--------------------~%" (car item))
                (format t "~A~%" (aux:maptree #'(lambda (x) (pretty-print :type :sign :format :text :form x)) derivation))
                (format t "~%------------------------------------------------~%"))))

        (aux:enum
          (funcall
            (if (*state* :uniq-parses)
                #'uniq-parses
                #'identity)
            (handler-case (parse expression)
              (simple-error (e)
                            (format t "~A" e))))
          1))
      "")))

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

  (uiop:chdir (*state* :project-path))

  (*state* :prompt (car (last (pathname-directory (*state* :project-path)))))

  (if (*state* :morphology) (setup-morph-analyzer (*state* :prompt)))

  (*state* :debug-lexicon-path (make-pathname :name "_lexicon" :type "lisp" :directory (pathname-directory (*state* :project-path))))
  (if (probe-file (*state* :debug-lexicon-path))
      (delete-file (*state* :debug-lexicon-path)))

  (*state* :lexicon-path (make-pathname :name (*state* :prompt) :type "lex" :directory  (pathname-directory (*state* :project-path))))
  (*state* :theory-path (make-pathname :name (*state* :prompt) :type "thr" :directory (pathname-directory (*state* :project-path))))
  (*state* :theory (aux:read-from-file (*state* :theory-path)))
  (*state* :features                (cdr (assoc 'features (*state* :theory))))
  (*state* :category-bundles (let ((bundles (cdr (assoc 'category-bundles (copy-alist (*state* :theory)))))
                                   (default-features (*state* :features)))
                               (mapcar
                                 #'(lambda (bundle)
                                     (let* ((bundle-features (cdr bundle))
                                            (missing-features (remove-if
                                                                        #'(lambda (feature)
                                                                            (member feature bundle-features :key #'car))
                                                                        default-features)))

                                       (append bundle (mapcar #'(lambda (x) (list x (gensym "?"))) missing-features))))
                                 bundles)))

  (*state* :feature-dictionary      (cdr  (assoc 'feature-dictionary (*state* :theory))))
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
    (proc-input (read))))
