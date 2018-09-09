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
  (make-texture-from-png "sprite.png"))

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
    (:idle ()
           (render)
           (sdl2:gl-swap-window win))
    (:quit ()
           t)))

(defun load-and-decode-png (filename)
  (with-open-file (input filename
                         :element-type '(unsigned-byte 8))
    (png:decode input)))

(defun png-to-texture-data (png)
  "Convert a libpng PNG object to OpenGL-compatible texture data."
  (let ((w (png:image-width png))
        (h (png:image-height png))
        (c (png:image-channels png)))
    (make-array (list (* w h c))
                :element-type (array-element-type png)
                :displaced-to png)))

(defun png-internal-format (png)
  (let ((c (png:image-channels png)))
    (ecase (png:image-bit-depth png)
      (8 (ecase c
           (1 :luminance8)
           (2 :luminance8-alpha8)
           (3 :rgb8)
           (4 :rgba8)))
      (16 (ecase c
            (1 :luminance16)
            (2 :luminance16-alpha16)
            (3 :rgb16)
            (4 :rgba16))))))

(defun png-format (png)
  (ecase (png:image-channels png)
    (1 :luminance)
    (2 :luminance-alpha)
    (3 :rgb)
    (4 :rgba)))

(defun png-data-type (png)
  (ecase (png:image-bit-depth png)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun make-texture-from-png (filename)
  (let* ((png (load-and-decode-png filename))
         (w (png:image-width png))
         (h (png:image-height png))
         (data (png-to-texture-data png))
         (level-of-detail 0)
         (internal-format (png-internal-format png))
         (border 0)
         (format (png-format png))
         (data-type (png-data-type png))
         (texture-id (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d :texture-2d
                     level-of-detail
                     internal-format
                     w
                     h
                     border
                     format
                     data-type
                    data)))
