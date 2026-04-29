;;; The shift-reduce parser works with a list of input signs and a
;;; stack of stacks.
;;;
;;; A parser state is a pair <Stack,Tape,History>.  shift moves the top
;;; tape symbol onto the stack; reduce combines the top two stack items
;;; via the reducer (typically CCG:COMBINE) when they admit a
;;; combination.

(defpackage :sr-parser
  (:use :common-lisp)
  (:export :parse))

(in-package :sr-parser)

(declaim (optimize (speed 3) (debug 1) (safety 1)))

(defun shift (state)
  "Shift the top symbol on tape to stack.  State -> State"
  (destructuring-bind (stack . tape) state
    (if (null tape)
        (error "Shift attempt for empty tape")
        (cons (cons (list (car tape)) stack)
              (cdr tape)))))

(defun reduce~ (state reducer)
  "Attempt to combine top two items in stack.

   Returns (values new-state ok-p):
     ok-p = T  if reducer succeeded; new-state is the post-reduce state.
     ok-p = NIL if reducer returned NIL (combination failed); new-state is NIL.
   Signals an error if the stack has fewer than two items."
  (let ((stack (car state))
        (tape (cdr state)))
    ;; O(1) check: is there a second cell on the stack?
    (when (or (endp stack) (endp (cdr stack)))
      (error "Reduce attempt on a short stack"))
    (let ((reduct (funcall reducer (caadr stack) (caar stack))))
      (if reduct
          (values (cons (cons (list reduct (cadr stack) (car stack))
                              (cddr stack))
                        tape)
                  t)
          (values nil nil)))))

(defun success-p (state)
  "State -> [NIL|Sign]"
  (let ((stack (car state))
        (tape (cdr state)))
    (and (null tape)
         (consp stack)
         (null (cdr stack))
         (car stack))))

(defun empty-tape-p (state)
  (endp (cdr state)))

(defun reducible-state-p (state)
  ;; "stack has at least two items": O(1) instead of O(n) length.
  (let ((stack (car state)))
    (and (consp stack) (consp (cdr stack)))))

(defun parse (agenda reducer &optional store)
  "A shift-reduce parser is a queue of parser states (agenda); the car
   of the agenda is the current state."
  (if (endp agenda)
      store
      (let ((state (car agenda)))
        (cond
          ;; Successful complete parse: harvest the top sign.
          ((success-p state)
           (parse (cdr agenda) reducer (cons (caar state) store)))

          ;; Tape exhausted: try to reduce as far as possible, otherwise
          ;; discard this branch.
          ((empty-tape-p state)
           (if (reducible-state-p state)
               (multiple-value-bind (new-state ok-p) (reduce~ state reducer)
                 (if ok-p
                     (parse (cons new-state (cdr agenda)) reducer store)
                     (parse (cdr agenda) reducer store)))
               (parse (cdr agenda) reducer store)))

          ;; Stack too short to reduce: must shift.
          ((not (reducible-state-p state))
           (parse (cons (shift state) (cdr agenda)) reducer store))

          ;; General case: try both reduce and shift.
          (t
           (multiple-value-bind (new-state ok-p) (reduce~ state reducer)
             (let ((shifted (shift state))
                   (rest (cdr agenda)))
               (if ok-p
                   (parse (cons new-state (cons shifted rest)) reducer store)
                   (parse (cons shifted rest) reducer store)))))))))
