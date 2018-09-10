;;;; battlefront.lisp

(in-package #:battlefront)

(require :sdl2)
(require :cl-opengl)

(defparameter *rot* 0)
(defparameter *sprite-tex* nil)
(defparameter *tileset-tex* nil)
(defparameter *tilemap* (make-array (list 10 10)))

(defun main ()
  "The entry point of our game."
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :title "Battlefront :: Alpha"
                           :w 640 :h 480 :flags '(:shown :opengl))
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
  (gl:viewport 0 0 640 480)
  (gl:matrix-mode :projection)
  (gl:ortho 0 640 480 0 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (setq *sprite-tex* (tex-png:make-texture-from-png "sprite.png"))
  (setq *tileset-tex* (tex-png:make-texture-from-png "tileset.png")))

(defun render ()
  ;; clear screen
  (gl:clear :color-buffer)
  ;; rotation
  (incf *rot* 0.2)
  (gl:load-identity)
  (draw-tilemap *tileset-tex* *tilemap* *rot* *rot*)
  (draw-sprite :texture *sprite-tex*
               :rgba (list
                      0.5
                      (cos (* 0.2 *rot*))
                      0.5
                      1.0)
               :x 320 :y 240
               :width 128 :height 128
               :rot *rot*
               :center-x 0.5 :center-y 0.5
               :scale-x (+ 1.2 (* 0.2 (sin (* 0.13 *rot*))))
               :scale-y (+ 1.2 (* 0.2 (sin (* 0.13 *rot*)))))
  (gl:flush))

(defun main-loop (win)
  "Run the game loop that handles input, rendering, etc"
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                    :scancode-escape)
                (sdl2:push-event :quit)))
    (:idle ()
           (render)
           (sdl2:gl-swap-window win))
    (:quit ()
           t)))

(defun draw-sprite (&key texture
                      (width 32.0) (height 32.0)
                      (x 0.0) (y 0.0)
                      (center-x 0.5) (center-y 0.5)
                      (rot 0.0)
                      (rgba '(1.0 1.0 1.0 1.0))
                      (scale-x 1.0) (scale-y 1.0))
  "Draw a 2D sprite to the screen."
  (let* ((w (* width scale-x))
         (h (* height scale-y))
         (bx (- (* w center-x) w))
         (fx (+ w bx))
         (by (- (* h center-y) h))
         (fy (+ h by)))
    (gl:bind-texture :texture-2d texture)
    (gl:push-matrix)
    (gl:translate x y 0)
    (gl:rotate rot 0 0 1)
    (gl:color (nth 0 rgba) (nth 1 rgba) (nth 2 rgba) (nth 3 rgba))
    (gl:begin :quads)
    (gl:tex-coord 0 0)
    (gl:vertex bx by)
    (gl:tex-coord 0 1)
    (gl:vertex bx fy)
    (gl:tex-coord 1 1)
    (gl:vertex fx fy)
    (gl:tex-coord 1 0)
    (gl:vertex fx by)
    (gl:end)
    (gl:pop-matrix)))

(defun draw-tilemap (texture tilemap x y)
  (let* ((tw (/ 32.0 512.0))
         (th (/ 32.0 32.0))
         (pox (mod x 32))
         (poy (mod y 32)))
    (gl:bind-texture :texture-2d texture)
    (gl:push-matrix)
    (gl:color 1.0 1.0 1.0 1.0)
    (gl:begin :quads)
    (dotimes (x 21)
      (dotimes (y 16)
        (let* ((tile-x (floor (/ x 32)))
               (tile-y (floor (/ y 32)))
               (tile-id (aref-2d tilemap tile-x tile-y))
               (u (* tile-id tw))
               (v 0)
               (px (- (* x 32) pox))
               (py (- (* y 32) poy)))
          (gl:tex-coord u v)
          (gl:vertex px py)
          (gl:tex-coord u (+ th v))
          (gl:vertex px (+ 32 py))
          (gl:tex-coord (+ tw u) (+ th v))
          (gl:vertex (+ 32 px) (+ 32 py))
          (gl:tex-coord (+ tw u) v)
          (gl:vertex (+ 32 px) py))))
    (gl:end)
    (gl:pop-matrix)))

(defun aref-2d (array x y)
  "Like aref, but for 2D arrays, and returns NIL if AT is out of bounds."
  (if (or (< x 0)
          (< y 0)
          (>= x (array-dimension array 0))
          (>= y (array-dimension array 1)))
      nil
      (aref array x y)))
