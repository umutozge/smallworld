#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :uiop :silent t)




(format t "~%Installing SmallWorld...~%~%")


(defparameter *current-dir* (uiop/os:getcwd))
(defparameter *install-dir*
  (or (uiop:directory-exists-p
        (uiop:native-namestring "~/.local/bin/"))
      (uiop:directory-exists-p
        (uiop:native-namestring "~/local/bin/"))
      (uiop:directory-exists-p
        (uiop:native-namestring "~/bin/"))))

(if (null *install-dir*)
    (setf *install-dir*
          (ensure-directories-exist (uiop:native-namestring "~/bin/"))))

(mapc
  (lambda (x)
    (format t "~vA ~a~%" 18 (first x) (second x)))
  `(("Operating system:" ,(uiop:operating-system))
    ("Lisp version:" ,(uiop:lisp-version-string))
    ("User dir:" ,(uiop:getenv "HOME"))
    ("Current dir:" ,*current-dir*)
    ("Install dir:" ,*install-dir*)))


(setf *default-pathname-defaults* (uiop:merge-pathnames*
                                    "code/"
                                    *current-dir*))

(load "main.lisp")

(format t "~%~%The executable 'smallworld' is created at ~%~%~A~%~%Make sure that this directory is in your PATH.~%~%" *install-dir*)

(sb-ext:save-lisp-and-die (uiop:merge-pathnames* "smallworld" *install-dir*)
	:toplevel #'main
	:executable t
	:compression nil)

(sb-ext:exit)
