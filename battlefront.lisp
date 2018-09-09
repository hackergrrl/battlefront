;;;; battlefront.lisp

(in-package #:battlefront)

(require :sdl2)
(require :cl-opengl)

(defparameter *rot* 0)

(defun main ()
  "The entry point of our game."
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :w 400 :h 400 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; Basic window/gl setup
        (setup-gl win gl-context)

        ;; Run main loop
        (main-loop win)))))

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with argument"
  (apply #'format t msg args)
  (finish-output))

(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:enable :texture-2d)
  (gl:viewport 0 0 400 400)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (tex-png:make-texture-from-png "sprite.png"))

(defun render ()
  ;; clear screen
  (gl:clear :color-buffer)
  ;; rotation
  (incf *rot* 0.5)
  (gl:load-identity)
  (gl:rotate *rot* 0 0 1)
  ;; quad
  (gl:begin :quads)
  (gl:color 1.0 1.0 1.0)
  (gl:tex-coord 0 0)
  (gl:vertex 0.0 0.0)
  (gl:tex-coord 0 1)
  (gl:vertex 0.0 1.0)
  (gl:tex-coord 1 1)
  (gl:vertex 1.0 1.0)
  (gl:tex-coord 1 0)
  (gl:vertex 1.0 0.0)  
  (gl:end)
  (gl:flush))

(defun main-loop (win)
  "Run the game loop that handles input, rendering, etc"
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
    (:idle ()
           (render)
           (sdl2:gl-swap-window win))
    (:quit ()
           t)))
