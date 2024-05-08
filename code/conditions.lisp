(define-condition default-feature-override (error)
  ((default :initarg :default
            :initform nil 
            :reader default)
   (overrider :initarg :overrider
              :initform nil 
              :reader overrider)))

(define-condition invalid-feature-value (error)
  ((feature :initarg :feature
            :initform nil 
            :reader feature)
   (value :initarg :value
              :initform nil 
              :reader value)))

(define-condition item-not-found (error)
  ((lexkey :initarg :lexkey
           :initform nil 
           :reader lexkey)))

(define-condition bad-syntactic-type (error)
  ((definition :initarg :definition
               :initform nil 
               :reader definition)))

(define-condition bad-semantic-interpretation (error)
  ((definition :initarg :definition
               :initform nil 
               :reader definition)))
