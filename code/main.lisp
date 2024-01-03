#!/opt/homebrew/bin/sbcl --script

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(load "models.lisp")
(load "uni-cg.lisp")
(load "pprinter.lisp")

(defparameter *vocab* nil)

(defun proc-input (input)
  (case input
		((:help :h)
		 (display-help))
		((:quit :q) (princ "bye, come again!") (terpri) (quit))
		((:refresh-model :rm)
		 (format t "Generating a random model...~%")
		 (funcall #'models:refresh-model))
		((:show-model :sm)
		 (models:display-model))
		((:interp-form :if)
		 (print (interpret-form)) (terpri))
		((:show-vocab :sv)
		 (print *vocab*) (terpri))
		((:switch-eta :se)
		 (princ (switch-eta-normalization)) (terpri))
		((:parse :p)
		 (terpri) (princ (parse-expr)) (terpri) (terpri))
		((:parse-file :pf)
		 (parse-file) (terpri))
		((:interp-exp :ie)
		 (print (interpret-expr)) (terpri) (terpri))
		(otherwise (princ "unknown command") (terpri))))

(defun interpret-form ()
	(models:interpret (read)))

(defun parse-file (&optional fname)
	(let* ((filename (or fname (read-line)))
        (sentences (aux:tsv-to-list
                     (make-pathname :name filename)))
        (outpath (aux:string-to-pathname filename ".out")))
          (with-open-file (str outpath :direction :output
                               :if-does-not-exist :create
                               :if-exists :overwrite)
            (dolist (i sentences)
              (format str (string-downcase "~{~A ~}~%~{~A~%~}~%~%") i (mapcar sign-sem (uniq-parses (parse-expr i)))))))) ; TODO can't display ambiguity

(defun parse-expr (&optional sent)
  (let* ((sentence (or sent (aux:string-to-list (read-line))))
         (unk (check-vocab sentence)))
    (if unk
        (format nil "~A is unknown" unk)
        (progn
          (mapc
            #'(lambda (sign)
               (format t "~A~%" sign)
               (format t "~%~A~%" (pprinter:print-text (sign-sem sign)))
               (format t "~%~A~%" (pprinter:print-tex (sign-sem sign)))
                )
            (uniq-parses (parse sentence)))
          ""))))

(defun interpret-expr ()
  (models:interpret (sign-sem (parse-expr))))

(defun display-help ()
  (format t "~%")
  (format t ":gen-model (:gm)		-- generate a random model~%")
  (format t ":show-model (:sm)		-- display the current loaded model~%")
  (format t ":refresh-model (:rm)		-- generate a new random model~%")
  (format t ":parse (:p) <sentence>	        -- parse the provided sentence into an applicative form~%")
  (format t ":interp-form (:if) <form>	-- interpret the provided applicative form~%")
  (format t ":interp-exp (:is) <expr>	-- parse and interpret the provided expression~%")
  (format t ":show-vocab (:sv)		-- display the vocabulary~%")
  (format t ":switch-eta (:se)		-- turn on/off eta-normalization of logical forms~%")
  (format t ":help (:h)			-- help~%")
  (format t ":quit (:q)			-- quit~%")
  (format t "~%"))


(defun check-vocab (sentence)
  (dolist (x sentence)
    (if (not (member x *vocab*))
        (return x))))

(defun switch-eta-normalization ()
  (setf *eta-normalize* (not *eta-normalize*)))

(defun command-line ()
  (or
    #+CLISP *args*
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    nil))

(defun main ()
  (let ((project-path (second (command-line))))
    (run-program "/usr/bin/clear" nil :output *standard-output*)
    (format t "Welcome to SmallWorld~%~%An educational software for computational natural langauge semantics~%Type :help for help, :quit for quit.~%")
    (format t "~%~%Initing parser...")
    (init-parser project-path)
    (setf *vocab* (funcall *lexicon* :keys))
    (format t "~%")
    (format t "done~%~%")
    (loop
      (format t "Ready> ")
      (finish-output)
      (proc-input (read)))))

(main)
