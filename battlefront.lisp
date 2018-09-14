;;;; battlefront.lisp

(in-package #:battlefront)

(require :sdl2)
(require :cl-opengl)

(defparameter *key-state* (make-hash-table))

(defparameter *rot* 0)
(defparameter *camera-x* 0)
(defparameter *camera-y* 0)
(defparameter *sprite-tex* nil)
(defparameter *tileset-tex* nil)
(defparameter *tilemap* (make-array (list 20 20)))
;; fun init:
;;(dotimes (n 400) (setf (row-major-aref *tilemap* n) (random 10)))

(defun key-down (key)
  (let ((scancode (intern (concatenate
                           'string "SCANCODE-" (string-upcase key)) "KEYWORD")))
    (gethash (sdl2:scancode-key-to-value scancode) *key-state*)))

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
        (setup-gl win gl-context)
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
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:viewport 0 0 640 480)
  (gl:matrix-mode :projection)
  (gl:ortho 0 640 480 0 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (setq *sprite-tex* (tex-png:make-texture-from-png "sprite.png"))
  (setq *tileset-tex* (tex-png:make-texture-from-png "tileset.png")))

(defun update ()
  (incf *rot* 0.2)
  (when (key-down :d) (incf *camera-x* 1.5))
  (when (key-down :a) (incf *camera-x* -1.5))
  (when (key-down :w) (incf *camera-y* -1.5))
  (when (key-down :s) (incf *camera-y* 1.5)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:load-identity)
  (draw-tilemap *tileset-tex* *tilemap* *camera-x* *camera-y*)
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
              (setf (gethash (sdl2:scancode-value keysym) *key-state*) t)
              (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                    :scancode-escape)
                (sdl2:push-event :quit)))
    (:keyup (:keysym keysym)
            (setf (gethash (sdl2:scancode-value keysym) *key-state*) nil))
    (:idle ()
           (update)
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
    (dotimes (i 21)
      (dotimes (j 16)
        (let* ((tile-x (+ i (floor (/ x 32))))
               (tile-y (+ j (floor (/ y 32))))
               (tile-id (aref-2d tilemap tile-x tile-y 9))
               (u (* tile-id tw))
               (v 0)
               (px (- (* i 32) pox))
               (py (- (* j 32) poy)))
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

(defun aref-2d (array x y default)
  "Like aref, but for 2D arrays, and returns DEFAULT if AT is out of bounds."
  (if (or (< x 0)
          (< y 0)
          (>= x (array-dimension array 0))
          (>= y (array-dimension array 1)))
      default
      (aref array x y)))
