;;; Smoke test for the consolidated LALR(1) generator.
;;; Run with: sbcl --script tests/test-lalr.lisp

(load "code/lalr.lisp")

(defparameter *failures* 0)

(defun check (name expected actual)
  (if (equal expected actual)
      (format t "  ok    ~A~%" name)
      (progn
        (incf *failures*)
        (format t "  FAIL  ~A~%        expected: ~S~%        actual:   ~S~%"
                name expected actual))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test 1: A trivial expression grammar.

(format t "~%Test 1: arithmetic-like grammar~%")

(defparameter *grammar-1*
  '((expr --> expr plus term  #'(lambda (e p tm) (declare (ignore p)) (list '+ e tm)))
    (expr --> term            #'(lambda (te) te))
    (term --> num             #'(lambda (n) (cadr n)))
    (term --> lp expr rp      #'(lambda (l e r) (declare (ignore l r)) e))))

(defparameter *parser-1*
  (lalr:make-parser *grammar-1* '(num plus lp rp) '$))

(defun tok-1 (cat &optional val) (cons cat (list cat val)))

(check "1 + 2"
       '(+ 1 2)
       (lalr:run *parser-1*
                 (list (tok-1 'num 1) (tok-1 'plus) (tok-1 'num 2) (tok-1 '$))))

(check "(1 + 2) + 3"
       '(+ (+ 1 2) 3)
       (lalr:run *parser-1*
                 (list (tok-1 'lp) (tok-1 'num 1) (tok-1 'plus) (tok-1 'num 2)
                       (tok-1 'rp) (tok-1 'plus) (tok-1 'num 3)
                       (tok-1 '$))))

(check "1 + 2 + 3 (left-associative)"
       '(+ (+ 1 2) 3)
       (lalr:run *parser-1*
                 (list (tok-1 'num 1) (tok-1 'plus) (tok-1 'num 2)
                       (tok-1 'plus) (tok-1 'num 3) (tok-1 '$))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test 2: Two parsers built simultaneously share no state.
;;; This is the key behavioral change vs. the old syn-parser/sem-parser
;;; split: with module-level globals you couldn't safely have two
;;; parsers coexist; with the closure-based generator you can.

(format t "~%Test 2: two parsers coexist~%")

(defparameter *grammar-a*
  '((s --> a       #'(lambda (a) (list :a a)))
    (a --> tok     #'(lambda (tk) (cadr tk)))))

(defparameter *grammar-b*
  '((s --> b       #'(lambda (b) (list :b b)))
    (b --> tok     #'(lambda (tk) (cadr tk)))))

(defparameter *parser-a* (lalr:make-parser *grammar-a* '(tok) '$))
(defparameter *parser-b* (lalr:make-parser *grammar-b* '(tok) '$))

(check "parser A does not interfere with parser B"
       :a
       (car (lalr:run *parser-a* (list (cons 'tok '(tok hello)) (cons '$ nil)))))

(check "parser B independently produces :b"
       :b
       (car (lalr:run *parser-b* (list (cons 'tok '(tok world)) (cons '$ nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summary

(format t "~%~%~A failure(s).~%" *failures*)
(if (zerop *failures*) (sb-ext:exit :code 0) (sb-ext:exit :code 1))
