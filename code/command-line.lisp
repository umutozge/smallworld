;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command-line options ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-morphology ()
  (car (aux:pathnames-by-extension "fst")))

(defun default-project ()
   (car (aux:pathnames-by-extension "yaml")))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h)
  (:name :morphology
   :default #'default-morphology 
   :description "provide the FST-FILE to use"
   :short #\m
   :long "morphology"
   :arg-parser #'(lambda (str)
                   (merge-pathnames (uiop:getcwd) (pathname str)))
   :meta-var "FST-FILE"
   )
  (:name :project
   :default #'default-project
   :arg-parser #'(lambda (str)
                   (merge-pathnames (uiop:getcwd) (pathname str)))
   :description "provide the PROJECT-YAML to use"
   :short #\p
   :long "project"
   :meta-var "PROJECT-YAML"))


;;;
;;; Some parts below are adapted from https://github.com/libre-man/unix-opts/blob/master/example/example.lisp
;;;

(defun handle-unknown-option (condition)
  (format t "~%WARNING: The option ~s is unknown and ignored.~%~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun parse-command-line ()
  (multiple-value-bind (options args)
    (handler-case 
      (handler-bind ((opts:unknown-option #'handle-unknown-option))
        (opts:get-opts))
      (opts:missing-arg (condition)
        (format t "Option ~s needs an argument!~%Run smallworld with -h for more information.~%"
                (opts:option condition))
        (opts:exit 1))
      (opts:arg-parser-failed (condition)
        (format t "Cannot parse ~s as argument of ~s~%"
                (opts:raw-arg condition)
                (opts:option condition))))
    (*state* :morphology (getf options :morphology))
    (*state* :project (getf options :project))
    (*state* :project-dir (make-pathname :directory (pathname-directory (*state* :project))))
    (when-option (options :help)
                 (opts:describe
                   :prefix "SmallWorld - A linguist's parser"
                   :suffix ""
                   :usage-of "smallworld")
                 (opts:exit 0))))
