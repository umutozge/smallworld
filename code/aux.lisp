;;;
;;; Some auxiliary functions
;;;

(defpackage :aux 
  (:use :common-lisp)
  (:export :multiset-table
           :flip-bool
           :gensym-p
           :uniq
           :tsv-to-list
           :empty-string-p
           :ensure-list
           :enum
           :string-to-list
           :csv-to-str-list
           :one-char-sym-p
           :shuffle-list
           :symbol=
           :translate-string-char
           :translate-string-word
           :partition
           :random-num-with-n-digits
           :read-file-as-string 
           :read-from-file
           :replace-char-with-str
           :restore-left-assoc
           :starts-with-p
           :string-to-pathname
           :strip-package-deco
           :symbol-char
           :symbol-length
           :symbol-starts-with
           :symbol-starts-with-char
           :readlist
           :prompt
           :write-string-to-file
           :tree2avm
           :tree2qtree))

(in-package aux)


(defmacro flip-bool (b)
  `(setf ,b (not ,b)))

(defun multiset-table (&optional (ht (make-hash-table :test #'equal)))
  "a closure based hash table implementation of alist

   (f key val)    -- adds, duplicates are allowed;
   (f key)        -- gets;
   (f :check key) --  checks existence
   (f :count)     -- get count
   (f :keys)      -- get the list of keys
   (f :get-table) -- get the embedded ht"

	#'(lambda (&rest input)
            (let ((head (car input))
                  (tail (cadr input)))
              (cond ((equal head :count) (hash-table-count ht))
                    ((equal head :keys)
                     (let ((store))
                       (maphash 
                         #'(lambda (k v)
                             (declare (ignore v))
                             (push k store))
                         ht)
                       (nreverse store)))
                    ((equal head :check)
                     (nth-value 1 (gethash tail ht)))
                    ((equal head :get-table) ht)
                    (t 
                     (if tail
                         (if (nth-value 1 (gethash head ht))
                             (setf (gethash head ht) (cons tail (gethash head ht)))
                             (setf (gethash head ht) (list tail)))
                         (if (nth-value 1 (gethash head ht))
                             (nth-value 0 (gethash head ht))
                             (error (format nil "~a is unknown." head)))))))))

(defun uniq (lst &optional (comparator #'equal))
  "like Unix uniq"
  (if (endp lst)
	nil
        (cons (car lst)
              (uniq (remove-if 
                               #'(lambda (x)
                                   (funcall comparator (car lst) x))
                               (cdr lst))
                    comparator))))

(defun tsv-to-list (path)
  "convert a tab-separated value file to a list"
  (labels ((text-to-list (text)
             (read-from-string 
               (concatenate 'string "(" text ")")))
           (read-lines (str &optional acc)
             (let ((line (read-line str nil 'eof)))
               (if (equal line 'eof)
                   acc
                   (read-lines 
                     str 
                     (append
                       acc
                       (list (text-to-list line))))))))
    (with-open-file (input-stream 
                      (if (typep path 'string)
                          (make-pathname :name path)
                          path)
                      :direction :input)
      (read-lines input-stream))))

(defun strip-package-deco (expr)
  (read-from-string
    (format nil "~A" expr)))

(defun starts-with-p (str prefix)
  "check whether the str starts with the prefix -- both are strings"
  (and 
    (>= (length str) (length prefix))
    (string-equal prefix (subseq str 0 (length prefix)))))

(defun readlist ()
  "Graham's On Lisp, p 56"
  (values (read-from-string
            (concatenate 'string
                         "(" (read-line) ")"))))







(defun prompt (&rest args)
  "Graham's On Lisp, p 56"
  (apply #'format *query-io* args)
  (read *query-io*))


;;;;;;;;;;;;;;;;;;
;; SYMBOL UTILS ::
;;;;;;;;;;;;;;;;;;


(defun symbol= (a b)
  "ignore their packages in comparin symbols"
  (string= (symbol-name a) (symbol-name b)))


(defun gensym-p (sym)
  (let ((name (symbol-name sym)))
    (and (digit-char-p (char name 1))
         (char= (char name 0) #\G)
         )))

(defun symbol-char (sym index)
  (let ((name (symbol-name sym)))
    (if (< index (length name))
        (char name index))))

(defun symbol-length (sym)
  (length (symbol-name sym)))

(defun symbol-starts-with (sym chr)
  (char= (char (symbol-name sym) 0) chr))

(defun symbol-starts-with-char (sym chr)
  (char= (char (symbol-name sym) 0) chr))

(defun one-char-sym-p (sym)
  (= 1 (length (symbol-name sym))))

(defun empty-string-p (str) 
  (and 
    (stringp str)
    (zerop (length str))))

;;;;;;;;;;;;;;;;
;;            ;;
;; List Utils ;;
;;            ;;
;;;;;;;;;;;;;;;;


(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun enum (lst &optional (index 0))
  (if (endp lst)
      nil
      (cons
        (list index (car lst))
        (enum (cdr lst) (+ index 1)))))

;;;;;;;;;;;;;;;
;;           ;;
;; I/O Utils ;;
;;           ;;
;;;;;;;;;;;;;;;

(defun string-to-pathname (&rest strings)
 (make-pathname :name (apply
                        #'concatenate 'string strings)))

(defun read-from-file (name)
  "return a list of lisp objects in the file at the pathname object
   also accepts a string name, but then the path construction is relative"
  (with-open-file (str (typecase name
                         (string (make-pathname :name name ))
                         (pathname name)
                         (t (error "READ-FROM-FILE raises type error")))
                       :direction :input)
	(do ((x (read str nil :eof) (read str nil :eof))
		 (store nil (cons x store)))
	  ((eq x :eof) (reverse store)))))

(defun write-string-to-file (string file &optional (policy :supersede))
  (with-open-file (str (etypecase file
                         (string (make-pathname :name file))
                         (pathname file))
                       :direction :output
                       :if-exists policy 
                       :if-does-not-exist :create)
    (format str "~a" string)))


(defun csv-to-str-list (pname &optional (field-marker #\,) (text-delimiter #\"))  
  "read a csv file into a list of string (cell) lists"
  (labels ((proc-line (line)
					  (let ((store nil)
							(cell-buffer (make-string 1000))
							(pos 0)
							(protect nil)) ; double quote delimited protect zone is initially inactive
						(do ((index 0 (+ index 1)))
						  ((= index (length line))
						   (reverse
							 (cons (subseq cell-buffer 0 pos) store)))
						  (let ((current-char (char line index)))
							(cond ((eq current-char text-delimiter) (flip-bool protect))
								  (protect 
									(setf (char cell-buffer pos) current-char)
									(incf pos))
								  ((eq current-char field-marker)
								   (push (subseq cell-buffer 0 pos) store)
								   (setf pos 0))
								  (t
									(setf (char cell-buffer pos) current-char)
									(incf pos)))))))) 
	  (with-open-file (str pname :direction :input)
		(do ((x (read-line str nil :eof) (read-line str nil :eof))
			 (store nil (cons (proc-line x) store)))
		  ((eq x :eof) (reverse store))))))

(defun string-to-list (str)
  " \"a b c\" => (a b c)"
  (read-from-string (concatenate 'string "(" str ")")))

(defun read-file-as-string (pname)
  "read the file at pname into a string without lisp-reading it"
  (with-open-file (str (typecase pname
                         (string (make-pathname :name pname))
                         (pathname pname)
                         (t (error "READ-FILE-AS-STRING type error")))
                       :direction :input) 
	(do ((line (read-line str nil :eof) (read-line str nil :eof))
		 (store "" (concatenate 'string store (format nil "~A~%" line))))
	  ((eq line :eof) store))))



;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; String/Symbol Utils ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun translate-string-char (str table)
  "translate the string str according to the character map table -- a dotted alist"
  (let ((master (copy-seq str)))
	(dotimes (index (length master) master)
	  (let ((replace-char (cdr (assoc (char master index) table))))
		(if replace-char
		  (setf (char master index) replace-char))))))

(defun translate-string-word (str table)
  "translate the string str according to the word map table -- a dotted alist"
  (apply
    #'concatenate
    'string
    (mapcar
      #'(lambda (word)
        (let ((val (assoc word table :test #'string-equal)))
          (if val (rest val) word)))
      (sb-unicode:words str))))

(defun random-pick (seq)
  "randomly pick an element from a sequence"
  (elt seq (random (length seq))))

(defun shuffle-list (lst &optional shuffled)
  (labels ((remove-first (item lst)
             (if (equal (car lst) item)
                 (cdr lst)
                 (cons (car lst) (remove-first item (cdr lst))))))

    (if (endp lst)
        shuffled
        (let ((pick (random-pick lst)))
          (shuffle-list (remove-first pick lst) (cons pick shuffled))))))

(defun replace-char-with-str (chr replace-str main-str)
  (let ((pos (position chr main-str)))
    (if pos
        (concatenate 'string 
                     (subseq main-str 0 pos)
                     replace-str
                     (subseq main-str (+ pos 1) (length main-str)))
        main-str)))

(defun restore-left-assoc (lst &optional store)
  "restore parentheses left associatively
   (a b c) => ((a b) c) "
  (cond ((endp lst) store)
        ((endp store) (restore-left-assoc (cdr lst) (list (car lst))))
        (t (restore-left-assoc (cdr lst) (list (list (car store) (car lst)))))))

(defun partition (lst size)
  "partition a list into chunks of size size"
  (labels ((expand-car (lst)
             (cond 
               ((endp lst) nil)
               ((listp (car lst))
                (if (null (cdr lst))
                    lst
                    (cons (append (car lst) (list (cadr lst)))
                          (cddr lst))))
               (t (cons (list (car lst)) (cdr lst)))))
           (_part (lst size count)
             (cond
               ((endp lst) nil)
               ((zerop count) (cons (car lst) (_part (cdr lst) size size)))
               (t (_part (expand-car lst) size (- count 1))))))
    (_part lst size size)))


(defun random-num-with-n-digits (n)
  (labels ((generate (guess n lowest-num)
                     (if (> guess lowest-num)
                       guess
                       (generate (random n) n lowest-num))))
    (let ((arg-to-random (expt 10 n))
          (lowest-num (expt 10 (- n 1))))
      (setf *random-state* (make-random-state t))
      (generate (random arg-to-random) arg-to-random lowest-num))))

;;;;;;;;;;;;;;;;;
;;;           ;;;
;;; TEX utils ;;;
;;;           ;;;
;;;;;;;;;;;;;;;;;

(defun tree2avm (tree &optional (indent "") (indent-increment "        "))
  "Convert a tree to AVM tex code as string"
  (labels ((print-tex (tree indent indent-increment)
             (cond ((null tree) "")
                   ((atom tree)
                    (format nil "~(~a~)" tree))
                   ((equal (car tree) 'continue)
                    (concatenate 'string
                                 (format nil " ~a~a~%" #\\ #\\)
                                 (format nil "~%~a" indent)
                                 (print-tex (cadr tree) indent indent-increment)
                                 (if (cddr tree)
                                     (print-tex (cons 'continue (cddr tree)) indent indent-increment)
                                     "")))
                   ((atom (car tree))
                    (concatenate 'string
                                 (format nil "~(~a~)  & ~(~a~)"
                                         (car tree) 
                                         (print-tex (cadr tree) (concatenate 'string indent indent-increment) indent-increment))))
                   (t
                    (concatenate 'string
                                 (format nil "~a[" #\\)
                                 (print-tex (car tree) indent indent-increment)
                                 (print-tex (cons 'continue (cdr tree)) indent indent-increment)
                                 (format nil "~a]" #\\))))))
    (concatenate 'string
                 (format nil "~abegin{avm}~%" #\\)
                 (print-tex tree indent indent-increment)
                 (format nil "~%~aend{avm}~%~%" #\\))))


(defun tree2qtree (tree)
  "Convert tree to a tikz-qtree as string"
  (labels ((print-tex (tree)
             (cond ((null tree) "")
                   ((atom tree)
                    (format nil "~a " tree))
                   ((equal (car tree) 'children)
                    (concatenate 'string
                                 (print-tex (cadr tree))
                                 (if (cddr tree)
                                     (print-tex (cons 'children (cddr tree)))
                                     "")))
                   (t
                    (format nil "[.{~a} ~a] "
                            (car tree)
                            (print-tex (cons 'children (cdr tree))))))))
    (concatenate 'string
                 (format nil "~aTree" #\\)
                 (print-tex tree)
                 (format nil "~%~%"))))
