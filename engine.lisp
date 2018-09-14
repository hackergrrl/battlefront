(in-package #:engine)

(defparameter *key-state* (make-hash-table))

(defun key-down (key)
  (let ((scancode (intern (concatenate
                           'string "SCANCODE-" (string-upcase key)) "KEYWORD")))
    (gethash (sdl2:scancode-key-to-value scancode) *key-state*)))

(defun init (&key title w h init update render)
  "The entry point of our game."
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :title title
                           :w w :h h :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (funcall init win gl-context)
        (main-loop win update render)))))

(defun main-loop (win update render)
  "Run the game loop that handles input, rendering, etc"
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (setf (gethash (sdl2:scancode-value keysym) *key-state*) t)
              (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                    :scancode-escape)
                (sdl2:push-event :quit)))
    (:keyup (:keysym keysym)
            (setf (gethash (sdl2:scancode-value keysym) *key-state*) nil))
    (:idle ()
           (funcall update)
           (funcall render)
           (sdl2:gl-swap-window win))
    (:quit ()
           t)))

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with argument"
  (apply #'format t msg args)
  (finish-output))
