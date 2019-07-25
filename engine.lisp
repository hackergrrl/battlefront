(defpackage #:engine
  (:use #:cl)
  (:export :init :key-down :mouse-pos))

(in-package #:engine)

(defparameter *frame-count* 0)
(defparameter *last-time-fps-recorded* 0)
(defparameter *key-state* (make-hash-table))
(defparameter *mouse-pos* (make-array 2))

(defun string+ (&rest strings)
  "Concatenate all string arguments."
  (apply #'concatenate (cons 'string strings)))

(defun key-to-scancode (key)
  "Converts a keyname string to an SDL scancode."
  (intern (string+ "SCANCODE-" (string-upcase key)) "KEYWORD"))

(defun key-down (key)
  (let ((scancode (key-to-scancode key)))
    (gethash (sdl2:scancode-key-to-value scancode) *key-state*)))

(defun mouse-pos (elm)
  (if (equal elm :x) (aref *mouse-pos* 0)
      (if (equal elm :y) (aref *mouse-pos* 1)
          nil)))

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
    (:mousemotion (:x x :y y)
                  (setf (aref *mouse-pos* 0) x
                        (aref *mouse-pos* 1) y))
    (:idle ()
           ;; FPS calculations
           (incf *frame-count*)
           (if (> (- (get-internal-real-time) *last-time-fps-recorded*) 1000)
               (progn
                 (debug-log "FPS: ~D~&" *frame-count*)
                 (setf *frame-count* 0
                       *last-time-fps-recorded* (get-internal-real-time))))

           ;; Update + render
           (funcall update)
           (funcall render)
           (sdl2:gl-swap-window win))
    (:quit ()
           t)))

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with argument"
  (apply #'format t msg args)
  (finish-output))
