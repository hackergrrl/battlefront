(in-package #:ecs)

(defparameter *ecs-systems* nil)
(defparameter *ecs-entities* nil)

(defun entity (&rest components)
  (push components *ecs-entities*)
  components)

(defun get-component-names (entity)
  (let ((n 0))
    (remove-if-not
     (lambda (e) (progn (incf n) (equal (mod n 2) 1)))
     entity)))

(defun query-entities (components)
  (remove-if-not
   (lambda (e)
     (subsetp components (get-component-names e) :test #'equal))
   *ecs-entities*))

(defun tick-systems ()
  (dolist (system *ecs-systems*)
    (dolist (e (query-entities (second system)))
      (funcall (third system) e))))

(defun strip-component-bindings (bindings)
  "Maps (:foo (b :bar) :bax) to (:foo :bar :bax)"
  (mapcar
   (lambda (b) (if (listp b) (second b) b))
   bindings))

(defmacro letify-bindings (e original-bindings &body forms)
  "Maps (:foo (b :bar) :bax) to a LET statement with bound vars."
  (let ((bindings (remove-if-not #'listp original-bindings)))
    `(let
         ,(mapcar
           (lambda (c) `(,(first c) (getf ,e ,(second c))))
           bindings)
       ,@forms)))

(defmacro defsystem (name components &body forms)
  `(let ((sys (list
               ,(string name)
               ,(cons 'list (strip-component-bindings (cdr components)))
               (lambda (,(car components))
                 (letify-bindings ,(car components) ,(cdr components) ,@forms)))))
    (replace-system ,(string name) sys)))

(defun system-equals? (sys1 sys2)
  (equal (first sys1) (first sys2)))

(defun replace-system (world system)
  (let ((pos (position system *ecs-systems* :test #'system-equals?)))
    (if (numberp pos)
        (setf (nth pos *ecs-systems*) system)
        (push system *ecs-systems*))))

(defmacro get* (obj &rest indicators)
  (if (null indicators)
      obj
      `(get* (getf ,obj ,(car indicators)) ,@(cdr indicators))))
