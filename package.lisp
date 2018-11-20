;;;; package.lisp

(defpackage #:ecs
  (:use #:cl)
  (:export :create-entity :defsystem
           :make-world :world-tick :world-entities :world-systems :world-p
           :entity-add-component :entity-components :getcmp
           :entity-id :entity-name :entity-p))

(defpackage #:tex-png
  (:use #:cl)
  (:export :make-texture-from-png))
