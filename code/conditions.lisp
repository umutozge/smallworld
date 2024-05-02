(define-condition default-feature-override (error)
  ((default :initarg :default
            :initform nil 
            :reader default)
   (overrider :initarg :overrider
              :initform nil 
              :reader overrider)))

(define-condition invalid-feature-value (error)
  ((feature :initarg :feature
            :initform 'aloha 
            :reader feature)
   (value :initarg :value
              :initform 'aloha 
              :reader value)))
