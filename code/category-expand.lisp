;;;
;;; Category expansion -- the bridge between a parsed syntactic
;;; category and a fully-specified attribute-value matrix.
;;;
;;; This module is consumed by SYN-GRAMMAR's reduce actions: when the
;;; LALR parser reduces an `acat fstr` into a TERM, the action calls
;;; EXPAND-BASE-CATEGORY to:
;;;
;;;   * pull in the default features defined in the project's
;;;     `category-bundles` (`(*state* :category-bundles)`),
;;;   * fill any unspecified features from the global
;;;     category template (`(*state* :category-template)`),
;;;   * validate user-supplied feature values against the project's
;;;     feature dictionary (`(*state* :feature-dictionary)`),
;;;   * resolve abbreviated values like `sg` to their (feature-name,
;;;     value) pair via reverse lookup,
;;;   * raise typed conditions on conflicts and unknown values.
;;;
;;; All project-state lookups go through (*STATE* :KEY) as defined in
;;; main.lisp.  This file used to live as a section in
;;; lexicon-reader.lisp; splitting it out keeps the YAML-reading
;;; orchestration in lexicon-reader thin.
;;;

(defun expand-base-category (cat)
  (labels ((feature-abrv-p (feat)
             (equal (car feat) 'fabv))

           (variable-p (sym)
             (and
               (symbolp sym)
               (char= #\? (aref (symbol-name sym) 0))))

           (feature-canceller-p (feat)
             (equal (cadr feat) 'fcancel))

           (update-fs (fs keyval)
             (let ((feature (car keyval))
                   (value (cadr keyval)))
               (cond ((endp fs) (list keyval))
                     ((equal feature (caar fs)) (if (not (variable-p (cadar fs)))
                                                    (error (make-condition 'default-feature-override
                                                                           :default feature
                                                                           :overrider value))
                                                    (cons keyval (cdr fs))))
                     (t (cons (car fs) (update-fs (cdr fs) keyval))))))

           (lookup-feature-name (fvalue)
             (let ((val  (rassoc fvalue (*state* :feature-dictionary) :test #'member)))
               (if val
                   (car val)
                   (error (make-condition 'invalid-feature-value :feature 'abbrv :value fvalue)))))

           (valid-value? (fname fvalue)
             (or
               (member fvalue (assoc
                                fname
                                (*state*
                                  :feature-dictionary)))
               (handler-case
                 (char= #\? (aref (symbol-name fvalue) 0))
                 (type-error (e)
                             (values nil e)))))

           (expand-feature (feat)
             (cond ((feature-abrv-p feat)
                    (let ((feat (cadr feat)))
                      (list (lookup-feature-name feat) feat)))
                   (t (let* ((name (car feat))
                             (value (cadr feat)))
                        (if (valid-value? name value)
                            feat
                            (error
                              (make-condition 'invalid-feature-value
                                              :feature name
                                              :value value)))))))

           (expand-cat (defined-features default-features)
             (reduce
               #'update-fs
               defined-features
               :initial-value default-features
               )))

    (let ((default-features (unifier:refresh-vars (cdr (assoc (car cat) (*state* :category-bundles)))))
          (defined-features (cdr cat)))
      (expand-cat (mapcar
                    #'expand-feature
                    defined-features)
                  (expand-cat default-features (unifier:refresh-vars (*state* :category-template)))
                  ))))
