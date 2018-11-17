;;;; package.lisp

(defpackage #:battlefront
  (:use #:cl))

(defpackage #:tex-png
  (:use #:cl)
  (:export :make-texture-from-png))

(defpackage #:engine
  (:use #:cl)
  (:export :init :key-down))

(defpackage #:ecs
  (:use #:cl)
  (:export :create-entity :defsystem
           :make-world :world-tick :world-entities :world-systems :world-p
           :entity-add-component :entity-components :entity-get-component
           :entity-id :entity-name :entity-p))
