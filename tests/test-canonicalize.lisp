;;; Smoke test for canonicalize-syn.
;;;
;;; The whole point of CANONICALIZE-SYN is that two structurally
;;; identical feature matrices that differ only in their gensym
;;; variable identities must produce EQUAL canonical forms.  Without
;;; this property, the COMBINE cache in ccg.lisp would never hit.
;;;
;;; This test stands alone (loads only unifier + the canonicalizer
;;; itself) so it can run without the full SmallWorld stack and its
;;; quicklisp dependencies.
;;;
;;; Run with: sbcl --script tests/test-canonicalize.lisp

(load "code/unifier.lisp")

(defparameter *failures* 0)

(defun check (name expected actual)
  (if (equal expected actual)
      (format t "  ok    ~A~%" name)
      (progn
        (incf *failures*)
        (format t "  FAIL  ~A~%        expected: ~S~%        actual:   ~S~%"
                name expected actual))))

;;; The canonicalizer copied verbatim from ccg.lisp so this test does
;;; not pull in the rest of the parser.

(defun canonicalize-syn (syn)
  (let ((table (make-hash-table :test 'eq))
        (counter -1))
    (labels ((walk (x)
               (cond
                 ((unifier::variablep x)
                  (or (gethash x table)
                      (setf (gethash x table)
                            (intern (format nil "?V~D" (incf counter))))))
                 ((consp x) (cons (walk (car x)) (walk (cdr x))))
                 (t x))))
      (walk syn))))

(format t "~%canonicalize-syn~%")

;;; 1. A category with no variables canonicalizes to itself.

(check "no-var category is identity"
       '((cat n) (agr sg) (bar 0))
       (canonicalize-syn '((cat n) (agr sg) (bar 0))))

;;; 2. Two refresh'd copies of the same template canonicalize equal.

(let ((c1 (unifier:refresh-vars '((cat n) (agr ?x) (bar 0))))
      (c2 (unifier:refresh-vars '((cat n) (agr ?x) (bar 0)))))
  (check "two refreshes of the same template are NOT equal raw"
         nil
         (equal c1 c2))
  (check "two refreshes of the same template ARE equal canonical"
         t
         (equal (canonicalize-syn c1) (canonicalize-syn c2))))

;;; 3. Variables in the same position get the same canonical name; in
;;;    different positions, different names.

(let* ((c (unifier:refresh-vars '((agr ?x) (case ?x) (gen ?y))))
       (canon (canonicalize-syn c)))
  (check "shared variables remain shared after canonicalization"
         (cadar canon) (cadadr canon))
  (check "distinct variables remain distinct after canonicalization"
         t (not (eql (cadadr canon) (cadr (caddr canon))))))

;;; 4. Traversal order is left-to-right: first var seen is ?V0.

(let ((canon (canonicalize-syn (unifier:refresh-vars '((agr ?a) (case ?b))))))
  (check "first variable becomes ?V0"
         '?v0 (cadar canon))
  (check "second distinct variable becomes ?V1"
         '?v1 (cadadr canon)))

(format t "~%~A failure(s).~%" *failures*)
(if (zerop *failures*) (sb-ext:exit :code 0) (sb-ext:exit :code 1))
