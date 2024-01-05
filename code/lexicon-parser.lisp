;;;
;;; Module for parsing CCG grammars into lexicons
;;;


;;; Input format:
;;;
;;; s\np : (lam x ($ x)) < cat dog


(defpackage :lexicon-parser
  (:import-from :common-lisp-user #:*state* #:lam #:forall #:exists)
  (:use :common-lisp)
  (:nicknames :lp)
  (:export :parse-lexicon))

(load "aux.lisp")
(load "read-macros.lisp")
(load "lalr.lisp")


(in-package lexicon-parser)

(defparameter *end-marker* '$)


;;;;;;;;;;;;;;;;;;;;;;;
;;;    Tokenizer    ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun read-entries ()
  "read the entries in the lexicon file to a list of strings"
  (reverse
    (remove-if
              #'(lambda (x) (or
                              (funcall 'aux:empty-string-p x)
                              (funcall 'aux:starts-with-p x ";")))
              (with-open-file (str (*state* :lexicon-path) :direction :input)
                (do ((entry (read-line str nil :eof) (read-line str nil :eof))
                     (store nil (cons entry store)))
                    ((eq entry :eof) store))))))

(defun split-entry (entry)
  (let ((buffer (make-string 1000))
        (pos 0)
        (category)
        (interpretation)
        (forms))
    (do ((index 0 (+ 1 index)))
        ((= index (length entry))
         (setf forms (string-trim '(#\Space #\Tab ) (subseq buffer 0 pos)))
         (list category interpretation forms))
        (let ((current-char (char entry index)))
          (case current-char
            (#\: (setf category (string-trim '(#\Space #\Tab) (subseq buffer 0 pos))) (setf pos 0))
            (#\< (setf interpretation (string-trim '(#\Space #\Tab) (subseq buffer 0 pos))) (setf pos 0))
            (otherwise (setf (aref buffer pos) current-char) (incf pos)))))))

(defun tokenize-category (cat)
  "tokenize the category given as string"
  (let ((store nil)
        (word-buffer (make-string 100))
        (pos 0))
    (do ((index 0 (+ index 1)))
        ((= index (length cat))
         (remove-if #'null
                    (reverse
                      (if (zerop pos)
                          store
                          (cons (intern (string-upcase (subseq word-buffer 0 pos))) store)))))
        (let ((current-char (char cat index)))
          (if (or (alpha-char-p current-char) (digit-char-p current-char))
              (progn
                (setf (aref word-buffer pos) current-char)
                (incf pos))
              (progn
                (if (not (zerop pos))
                    (let* ((word (string-upcase (subseq word-buffer 0 pos)))
                           (int? (parse-integer word :junk-allowed t)))
                      (push
                        (or int?
                            (intern word))
                        store)
                      (setf pos 0)))
                (push
                  (case current-char
                    (#\( 'OP)
                    (#\) 'CP)
                    (#\[ 'OB)
                    (#\] 'CB)
                    (#\/ 'FS)
                    (#\\ 'BS)
                    (#\= 'EQ)
                    (#\+ '+)
                    (#\- '-))
                  store)))))))

(defun parse-sem (sem)
  (labels ((lambda-p (expr)
             (and
               (consp expr)
               (= (length expr) 3)
               (print (member (print (car expr)) '(lam forall exists)))))
           (r-adjoin (expr adjunct)
             (if (null adjunct)
                 expr
                 (list expr adjunct)))
           (parse (sem)
             (cond ((atom sem)
                    sem)
                   ((endp sem)
                    nil)
                   ((= 1 (length sem))
                    (parse (car sem)))
                   ((lambda-p sem)
                    (list (car sem) (cadr sem) (parse (cddr sem))))
                   (t
                    (r-adjoin
                      (list
                        (parse (car sem))
                        (parse (cadr sem)))
                      (parse (cddr sem)))))))
    (let ((newsem (aux:translate-string-char (print sem) '((#\\ . #\!) (#\' . #\Space)))))
      (parse (aux:string-to-list newsem)))))


(defun generate-entry-list ()
  (mapcar
    #'(lambda (x)
        (let ((syn (car x))
              (sem (cadr x))
              (tokens (caddr x)))
          (list
            (parse-category (tokenize-category syn))
            (parse-sem sem)
            (aux:string-to-list tokens))))
    (mapcar
      #'split-entry
      (read-entries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Parsing       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *lexicon* '((ob ob)
                          (cb cb)
                          (op op)
                          (cp cp)
                          (eq eq)
                          (fs slash)
                          (bs slash)
                          ($ $)
                          ))

(eval
  (let ((grammar
          '((cat --> acat fstr 		  #'(lambda (acat fs) (expand-base-category (cons (cadr acat) fs))))
            (cat --> op cat cp 		  #'(lambda (op cat cp) cat))
            (cat --> cat slash cat	  #'(lambda (cat1 slash cat2) (list (list 'in cat2) (list 'dir (expand-slash slash)) (list 'out cat1))))
            (fstr --> ob feat f cb        #'(lambda (ob feat f cb) (cons feat f)))
            (fstr -->         		  #'(lambda () nil))
            (f -->          		  #'(lambda () nil))
            (f --> feat f       	  #'(lambda (feat f) (cons feat f)))
            (feat --> fname eq feat       #'(lambda (fname eq feat) (list (cadr fname) (cadr feat))))
            (feat --> fval      	  #'(lambda (fval) (list 'fabv (cadr fval))))) 
          )
        (lexforms
          '(acat ob cb op cp slash eq fname fval)))
    (lalr:make-parser grammar lexforms '$)))

(defun parse-category (words)
  (let ((new-words (append words (list *end-marker*))))
    (labels ((lookup (word)
               (cadr (assoc word *lexicon*)))
             (next-input ()
               (let* ((word (pop new-words))
                      (cat (lookup word)))
                 (cons cat                  ; category
                       (list cat word))))   ; value
             (parse-error ()
               (format nil "Error before ~a" new-words)))
      (lalr:lalr-parser #'next-input #'parse-error))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Category expansion    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"This section works on the outputs of the lalr-parser, turning them into lexical categories and writing them to the debug lexicon file. That file will be read by uni-cg module to construct the online lexicon for parsing linguistic input."

(defun expand-slash (slash)
  (case (cadr slash)
    (fs 'forward)
    (bs 'backward)))

(defun expand-base-category (cat)
  (labels ((feature-abrv-p (feat)
             (equal (car feat) 'fabv))
           (lookup-feature-name (fvalue)
             (let ((val  (rassoc fvalue (*state* :feature-dictionary) :test #'member)))
               (if val
                   (car val)
                   (error (format nil "ERROR: Feature value ~a is unknown." fvalue)))))
           (expand-feature (feat)
             (if (feature-abrv-p feat)
                 (let ((feat (cadr feat)))
                   (list (lookup-feature-name feat) feat))
                 (let* ((name (car feat))
                        (value (cadr feat))
                        (value-valid? (member value (assoc
                                                      name
                                                      (*state*
                                                        :feature-dictionary)))))
                   (if value-valid?
                       feat
                       (error (format nil "ERROR: ~a is an invalid value for ~a." value name))))))
           (update-cat-with-feature (cat feat)
             (let ((exists-at (position (car feat) cat :key #'car)))
               (if exists-at
                   (progn
                     (setf (nth exists-at cat) feat)
                     cat)
                   cat)))
           (expand-cat (cat features)
             (if (endp features)
                 cat
                 (expand-cat
                   (update-cat-with-feature cat (car features))
                   (cdr features)))))
    (let ((cat-features (cdr (assoc (car cat) (*state* :category-bundle-symbols))))
          (add-features (cdr cat)))
      (expand-cat (copy-list (*state* :base-cat-template)) (append cat-features 
                                                                                     (mapcar 
                                                                                       #'expand-feature
                                                                                       add-features))))))

(defun proc-entries (entries)
  (labels ((build-entry (phon syn sem)
             `((phon ,phon)
               (syn ,syn)
               (sem ,(sublis (list (cons 'common-lisp-user::$ phon)) sem)))))
    (with-open-file (str (*state* :debug-lexicon-path) :direction :output)
      (dolist (entry entries)
        (let ((syn (car entry))
              (sem (cadr entry))
              (tokens (caddr entry)))
          (dolist (token tokens)
            (format str "~A~%~%" (build-entry token syn sem))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Main drive       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-lexicon ()
  (format t "~%Parsing the lexicon found at ~a . . .~%"
          (pathname-name (*state* :lexicon-path)))
  "then add items to the lalr *lexicon* on the basis of *feature-dictionary*"
  (dolist (x (*state* :feature-dictionary))
    (dolist (y (cdr x))
      (push (list y 'fval) *lexicon*))
    (push (list (car x) 'fname) *lexicon*))
  "then add *category-bundle-symbols* as  acat items to the lalr *lexicon*"
  (dolist (x (*state* :category-bundle-symbols))
    (push (list (car x) 'acat) *lexicon*))
  (proc-entries
    (generate-entry-list))
  )
