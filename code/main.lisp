;#!/opt/homebrew/bin/sbcl --script

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(load "~/.sbclrc")
(require "str")
;(ql:quickload :str :silent t)

(load "pprinter.lisp")

(defun proc-input (input)
  (case input
    ((:help :h)
     (display-help))
    ((:quit :q) (princ "bye, come again!") (terpri) (quit))
    ((:show-vocab :sv)
     (format t "~{~{~6S~^ ~}~%~}" (aux:partition (*state* 'vocab) 5)) (terpri))
    ((:switch-eta :se)
     (princ (toggle-flag :eta-normalization)) (terpri))
    ((:switch-derivation :sd)
     (princ (toggle-flag :derivation)) (terpri))
    ((:switch-uniq-parses :su)
     (princ (toggle-flag :uniq-parses)) (terpri))
    ((:parse :p)
     (terpri) (princ (parse-expr)) (terpri) (terpri))
    ((:parse-file :pf)
     (parse-file) (terpri))
    ((:reload :rl) (main))
    (otherwise (princ "unknown command") (terpri))))

(defun display-help ()
  (let ((data '((":parse (:p) <sentence>" "parse the provided sentence into an applicative form")  
                (":show-vocab (:sv)" "display the vocabulary")  
                (":switch-derivation (:sd)" "turn on/off display of derivations")  
                (":switch-eta (:se)" "turn on/off eta-normalization of logical forms") 
                (":switch-uniq-parses (:su)" "turn on/off eliminating semantically spurious parses") 
                (":reload (:rl)" "reload the system") 
                (":help (:h)" "help") 
                (":quit (:q)" "quit"))))
  (format t "~%")
  (format t "~{~{~24A~^ -- ~} ~%~}" data)
  (format t "~%")))

(defun parse-file (&optional fname)
  (let* ((filename (or fname (read-line)))
         (sentences (aux:tsv-to-list
                      (make-pathname :name filename)))
         (outpath (aux:string-to-pathname filename ".out")))
    (with-open-file (str outpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (dolist (i sentences)
        (format str (string-downcase "~{~A ~}~%~{~A~%~}~%~%")
                i
                (mapcar sign-sem (uniq-parses (parse-expr i)))))))) ; TODO can't display ambiguity

(defun parse-expr (&optional sent)
  (let* ((sentence (or sent (aux:string-to-list (read-line))))
         (unk (check-vocab sentence)))
    (if unk
        (format nil "~A is unknown" unk)
        (progn
          (mapc
            #'(lambda (item)
                (format t "~%--------------------PARSE ~D--------------------~%" (car item))
                (format t "~A~%" (caadr item))
                (format t "~%~A~%" (pprinter:print-text (sign-sem (caadr item))))
                (format t "~%~A~%" (pprinter:print-tex (sign-sem (caadr item))))
                (format t "~%------------------------------------------------~%")
                (when (*state* :derivation) 
                  (format t "~%--------------------DERIV ~D--------------------~%" (car item))
                  (format t "~A~%" (cadr item))
                  (format t "~%------------------------------------------------~%")))
            (aux:enum
              (funcall
                (if (*state* :uniq-parses)
                    #'uniq-parses
                    #'identity)
                (parse sentence))
              1))
          ""))))


(defun check-vocab (sentence)
  (dolist (x sentence)
    (if (not (member x (*state* 'vocab)))
        (return x))))

(defun toggle-flag (flag)
  (*state* flag (not (*state* flag))))


(defun switch-eta-normalization ()
  (*state* :eta-normalize (not (*state* :eta-normalize))))

(defun switch-uniq-parses ()
  (*state* :uniq-parses (not (*state* :uniq-parses))))

(defun command-line ()
  (or
    #+CLISP *args*
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    nil))

(defun main ()
  ;; init a global *state* closure available everywhere
  (setf  (symbol-function '*state*)
         (let ((table (make-hash-table)))
           #'(lambda (key &optional (value '<NULL>))
               (if (not (equal value '<NULL>) )
                   (setf (gethash key table) value)
                   (multiple-value-bind (val status)
                     (gethash key table)
                     val)))))

  ;; decorate *state*
  (*state* :eta-normalize t)
  (*state* :derivation nil)
  (*state* :uniq-parses nil)
  (*state* :lexicon (aux:multiset-table))
  (*state* :project-path (cadr (command-line)))
  (*state* :prompt (car (last (str:split #\/ (*state* :project-path)))))
  (*state* :debug-lexicon-path (aux:string-to-pathname (*state* :project-path) "/_lexicon.lisp" ))
  (if (probe-file (*state* :debug-lexicon-path))
      (delete-file (*state* :debug-lexicon-path)))
  (*state* :lexicon-path (aux:string-to-pathname (*state* :project-path) "/lexicon.lisp" ))
  (*state* :theory-path (aux:string-to-pathname (*state* :project-path) "/theory.lisp"))
  (format t "~%nReading the theory found at ~a . . .~%" (pathname-name (*state* :theory-path)))
  (*state* :theory (aux:read-from-file (*state* :theory-path)))
  (*state* :base-cat-template       (cadr (assoc 'base-cat-template (*state* :theory))))
  (*state* :feature-dictionary      (cdr  (assoc 'feature-dictionary (*state* :theory))))
  (*state* :category-bundle-symbols (cdr  (assoc 'category-bundle-symbols (*state* :theory))))


  (run-program "/usr/bin/clear" nil :output *standard-output*)
  (format t "Welcome to SmallWorld~%~%A linguists' parser based on CCG~%~%Type :help for help, :quit for quit.~%")
  (format t "~%Initing parser...")

  (load "ccg.lisp")
  (init-parser)

  (*state* 'vocab (funcall (*state* :lexicon) :keys))
  (format t "~%")
  (format t "done~%~%")
  (loop
    (format t "~a> " (*state* :prompt))
    (finish-output)
    (proc-input (read))))

(main)
