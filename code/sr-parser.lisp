;;; The shift-reduce parser works with a list of input signs and a
;;; stack of stacks.


;;; A parser state is a pair <Stack,Tape,History>.
;;; Define two actions shift and reduce that works on parser states

(defpackage :sr-parser
  (:use :common-lisp)
  (:export :parse))

(in-package sr-parser)


(defun shift (state)
  "shift the top symbol on tape to stack
   State -> State
   "
  (destructuring-bind
    (stack . tape)
    state
    (if (null tape)
        (error "Shift attempt for empty tape")
        (cons
          (cons (list (car tape)) stack)
          (cdr tape)))))

(defun reduce~ (state reducer)
  "attempt to combine top two items in stack; raise error if stack is too short; return nil for combination failure"
  (let ((stack (car state))
        (tape (cdr state)))
    (if (< (length stack) 2)
        (error "Reduce attempt on a short stack")
        (let ((reduct (funcall reducer (caadr stack) (caar stack))))
          (if reduct
              (cons
                (cons (list reduct (cadr stack) (car stack)) (cddr stack))
                tape)
              )))))

(defun success-p (state)
  "State -> [NIL|Sign]"
  (let ((stack (car state))
        (tape (cdr state)))
    (and
      (null tape)
      (consp stack)
      (null (cdr stack))
      (car stack))))

(defun empty-tape-p (state)
  (endp (cdr state)))

(defun reducible-state-p (state)
  (> (length (car state)) 1))


(defun parse (agenda reducer &optional store)
  "a shift-reduce parser is a queue of parser states (agenda)
   car of the agenda is the current state
 "
  (if (endp agenda)
      store
      (let* ((state (car agenda)))
        (cond ((success-p state)
               (parse (cdr agenda)
                         reducer
                         (cons (caar state) store)))
              ((empty-tape-p state) ;tape is over
               (if (reducible-state-p state) ; is stack still reducible
                   (parse                  ; yes, then reduce it
                     (cons (reduce~ state reducer) (cdr agenda))
                     reducer
                     store)
                   (parse
                     (cdr agenda)
                     reducer
                     store))) ; no, discard the current state and go on
              ((not (reducible-state-p state))    ; compulsory shift
               (parse
                 (cons (shift state) (cdr agenda))
                 reducer
                 store))
              (t
               (let ((reduct (reduce~ state reducer))
                     (shifted (shift state)))
                 (if reduct
                     (parse (cons
                                 reduct
                                 (cons shifted (cdr agenda)))
                               reducer
                               store)
                     (parse (cons
                                 shifted
                                 (cdr agenda))
                               reducer
                               store))))))))
