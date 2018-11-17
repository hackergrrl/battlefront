(in-package #:ecs)

;; Q: how could I test that I exported a symbol from a package correctly?
;; A: eval the defpackage expr and try to access ecs:foobar from the CL-USER package in the REPL

;; Q: can you export defstructs? does that make sense?
;; A: best answer I could find is: do tab completion on ecs::entity-<TAB> and add each of those dynamic function symbols to the exports
;; A: I found a function called EXPORT that maybe does dynamic exports? could write a macro that exports a defstruct?

;; Q: can I reset the sbcl lisp image? I don't know what is currently loaded/not loaded.

;; Q: if I change a defpackage to include a new symbol to export, do I need to re-eval anything else to make it visible?
;; A: no, it doesn't seem so. other than, of course, its definition in the source file

;; Q: when I eval stuff in emacs using eg. C-c C-c, what package does it get loaded into? The one that my REPL is in? Into whatever 'in-package' is at the top of the file?
;; A: evaluate *PACKAGE*: it will tell you the current package

;; GOAL: write DEFSYSTEM macro

;; public

(defstruct world
  (max-id 0)
  entities
  systems)

(defstruct entity
  (name "an entity")
  id
  components)

(defun world-tick (world)
  (dolist (sys (world-systems world))
    (format t "running system ~s~%" (first sys))
    (dolist (e (apply #'world-query (cons world (second sys))))
      (format t "running system ~s on entity ~s~%" (first sys) (entity-id e))
      (funcall (third sys) e))))

(defun create-entity (world &key (name "an entity") (components nil))
  (incf (world-max-id world))
  (let ((entity (make-entity :name name :id (world-max-id world) :components components)))
    (push entity (world-entities world))
    entity))

(defun entity-add-component (entity &rest components)
  (setf (entity-components entity)
        (append (entity-components entity) components)))

(defun entity-get-component (entity cname)
  (first (remove-if-not
          (lambda (c) (equal (string (type-of c)) (string cname)))
          (entity-components entity))))

(defun world-query (world &rest cs)
  (let ((css (mapcar 'string cs))
        (res nil))
    (dolist (e (world-entities world))
      (when (subsetp
             css
             (entity-components-names e)
             :test #'equal)
        (push e res)))
    res))

;; private

(defun entity-components-names (e)
  (mapcar
   (lambda (c) (string (type-of c)))
   (entity-components e)))

(defun add-system (world name components fn)
  (let ((system (list name (mapcar 'string components) fn)))
    (replace-system world system)))

(defun system-equals? (sys1 sys2)
  (equal (first sys1) (first sys2)))

(defun replace-system (world system)
  (let ((pos (position system (world-systems world) :test #'system-equals?)))
    (if (numberp pos)
        (setf (nth pos (world-systems world)) system)
        (push system (world-systems world)))))

;; TODO: defsystem macro
;; - check if system already exists: replace it; otherwise add

;;-----------------------------------

;; example

;; Q: any way to skip needing specify the WORLD instance here in DEFSYSTEM?

;;(defsystem gravity (e :pos :physics)
;;  (decf (physics-z-vel (entity-get-component e :physics)) 0.01))

(defun system-gravity (e)
  (decf (physics-z-vel (entity-get-component e :physics)) 0.01))

(defstruct pos
  (x 0)
  (y 0)
  (z 0))

(defstruct physics
  (x-vel 0)
  (y-vel 0)
  (z-vel 0))
