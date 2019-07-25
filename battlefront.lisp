(in-package #:battlefront)

;;--------------------------------------------------------------------------------
;; PLANNING
;;--------------------------------------------------------------------------------

;;; GOAL: ???
;; [ ] ???

;;; QUESTIONS
;; Q: how to ignore a param when writing a lambda?
;; A: (lambda (x) (declare (ignore x)) 69)

;;--------------------------------------------------------------------------------
;; HELPER FUNCTIONS
;;--------------------------------------------------------------------------------

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
         (pox (mod (- x (/ *screen-width* 2)) 32))
         (poy (mod (- y (/ *screen-height* 2)) 32)))
    (gl:bind-texture :texture-2d texture)
    (gl:push-matrix)
    (gl:color 1.0 1.0 1.0 1.0)
    (gl:begin :quads)
    (dotimes (i 21)
      (dotimes (j 16)
        (let* ((tile-x (+ i (floor (/ (- x (/ *screen-width* 2)) 32))))
               (tile-y (+ j (floor (/ (- y (/ *screen-height* 2)) 32))))
               (tile-id (aref-2d tilemap tile-x tile-y 10))
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

(defun nvunit-safe (v)
  "Normalizes \"v\" without crashing when the zero-vector is given."
  (if (equal 0.0 (vlength v))
      v
      (nvunit v)))

(defun screen-xy-to-rot (sx sy)
  "Convert screen coordinates to an angle from the center of the screen."
  (let ((x (- sx (/ *screen-width* 2.0)))
        (y (- sy (/ *screen-height* 2.0))))
    (atan y x)))

(defun rad2deg (r)
  (* r (/ 180.0 PI)))


(defun plist-props (p)
  "List of all property names on a plist."
  (let ((n 0))
    (remove-if-not
     (lambda (x)
       (declare (ignore x))
       (equal 1 (mod (incf n) 2)))
     p)))

(defun plist-assign (base new)
  "Assign keys from plist NEW atop plist BASE."
  (let ((res (copy-list base)))
    (dolist (prop (plist-props new))
      (setf (getf res prop) (getf new prop)))
    res))

(defun make-physics (&rest vals)
  (plist-assign (list :pos (vec3 0 0 0)
                      :vel (vec3 0 0 0)
                      :rot 0)
                vals))

(defun texture (name)
  (gethash name *textures*))

;;--------------------------------------------------------------------------------
;; GLOBALS
;;--------------------------------------------------------------------------------

(require :sdl2)
(require :cl-opengl)

(defparameter *player* (entity :physics (make-physics :pos (vec3 320 240 0))
                               :player-controller t
                               :sprite (list :texture "player")))
(defparameter *camera* (entity :physics (make-physics :pos (vec3 0 0 0))
                               :camera (list :target *player*)))

(defparameter *textures* (make-hash-table))
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *tilemap* (make-array (list 20 20)))

;;--------------------------------------------------------------------------------
;; SYSTEMS
;;--------------------------------------------------------------------------------

(defsystem 2d-physics (e (p :physics))
  (let ((gnd-friction 0.93))
    ;; move pos by velocity
    (nv+ (getf p :pos) (getf p :vel))
    ;; apply ground friction
    (nv* (getf p :vel) gnd-friction)))

(defsystem player-controller (e (p :physics) :player-controller)
  (let* ((speed 0.15)
         (input (vec3
                 (* speed (+
                           (if (engine:key-down :d) 1 0)
                           (if (engine:key-down :a) -1 0)))
                 (* speed (+
                           (if (engine:key-down :w) -1 0)
                           (if (engine:key-down :s) 1 0)))
                 0))
         (force (v* (nvunit-safe input) speed)))
    (nv+ (getf p :vel) force)
    (setf (getf p :rot)
          (screen-xy-to-rot (engine:mouse-pos :x)
                            (engine:mouse-pos :y)))))

(defsystem follow-camera-target (e (p :physics) (c :camera))
  (let ((target (getf c :target)))
    (if (not (null target))
        (let ((pos (getf p :pos))
              (tpos (get* target :physics :pos)))
          (nv+ pos (v* (v- tpos pos) 0.3))))))

;;--------------------------------------------------------------------------------
;; ENGINE LOGIC
;;--------------------------------------------------------------------------------

(defun main ()
  (engine:init :title "Battlefront"
               :w *screen-width* :h *screen-height*
               :init 'init
               :update 'update
               :render 'render))

(defun init (win gl-context)
  "Setup OpenGL with the window WIN and the gl context GL-CONTEXT"
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
  ;; empty arena
  (dotimes (x 20)
    (dotimes (y 20)
      (if (and (> x 0) (> y 0) (< x 19) (< y 19))
          (setf (aref *tilemap* x y) 8)
          (setf (aref *tilemap* x y) 0))))
  ;; load textures
  (setf
   (gethash "player" *textures*) (tex-png:make-texture-from-png "player.png")
   (gethash "tiles" *textures*) (tex-png:make-texture-from-png "tileset.png")))

(defun update ()
  (tick-systems))

(defun render ()
  (gl:clear :color-buffer)
  (gl:load-identity)
  (let* ((cam-pos (get* *camera* :physics :pos)))
    (draw-tilemap (texture "tiles") *tilemap* (vx cam-pos) (vy cam-pos))
    (dolist (e (query-entities '(:sprite)))
      (let ((draw-pos (v+ (v- (get* e :physics :pos) cam-pos)
                          (vec3 320 240 0))))
        (draw-sprite :texture (texture "player")
                     :rgba '(1 1 1 1)
                     :x (vx3 draw-pos)
                     :y (vy3 draw-pos)
                     :width 48 :height 48
                     :rot (rad2deg (get* e :physics :rot))
                     :center-x 0.5 :center-y 0.5))))
  (gl:flush))

