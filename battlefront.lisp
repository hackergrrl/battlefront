(defpackage #:battlefront
  (:use #:cl #:3d-vectors))

(in-package #:battlefront)

;; GOAL: add components + systems s.t. player can be controlled with keyboard

;; [x] basic ecs.lisp integration
;; [x] load in skirmish online player sprite
;; [x] WASD movement controls
;; [x] use vectors
;; [x] normalize input direction
;; [x] mouse-look rotation
;; [x] camera component + camera entity + target follow
;; [ ] draw-sprite component + render function + render query pass

(require :sdl2)
(require :cl-opengl)

(defstruct physics
  (pos (vec3 0 0 0))
  (vel (vec3 0 0 0))
  (rot 0))

(defstruct plr-controller)

(defstruct camera
  (target nil))

(defparameter *player* (ecs:create-entity (list
                                           (make-physics :pos (vec3 320 240 0))
                                           (make-plr-controller))))
(defparameter *camera* (ecs:create-entity (list
                                           (make-physics :pos (vec3 0 0 0))
                                           (make-camera :target *player*))))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *rot* 0)
(defparameter *sprite-tex* nil)
(defparameter *tileset-tex* nil)
(defparameter *tilemap* (make-array (list 20 20)))
;; fun init:
;;(dotimes (n 400) (setf (row-major-aref *tilemap* n) (random 10)))

(ecs:defsystem 2d-physics (e :physics)
               (let ((p (ecs:getcmp :physics e))
                     (gnd-friction 0.93))
                 ;; move pos by velocity
                 (nv+ (physics-pos p) (physics-vel p))
                 ;; apply ground friction
                 (nv* (physics-vel p) gnd-friction)))

(ecs:defsystem player-controller (e :physics :plr-controller)
               (let* ((p (ecs:getcmp :physics e))
                      (speed 0.15)
                      (input (vec3
                               (* speed (+
                                         (if (engine:key-down :d) 1 0)
                                         (if (engine:key-down :a) -1 0)))
                               (* speed (+
                                         (if (engine:key-down :w) -1 0)
                                         (if (engine:key-down :s) 1 0)))
                               0))
                      (force (v* (nvunit-safe input) speed)))
                 (nv+ (physics-vel p) force)
                 (setf (physics-rot p) (screen-xy-to-rot (engine:mouse-pos :x) (engine:mouse-pos :y)))))

(ecs:defsystem follow-camera-target (e :physics :camera)
               (let ((target (camera-target (ecs:getcmp :camera e))))
                 (if (not (null target))
                     (let ((pos (physics-pos (ecs:getcmp :physics e)))
                           (tpos (physics-pos (ecs:getcmp :physics target))))
                       (nv+ pos (v* (v- tpos pos) 0.3))))))

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
  (setq *sprite-tex* (tex-png:make-texture-from-png "player.png"))
  (setq *tileset-tex* (tex-png:make-texture-from-png "tileset.png")))

(defun update ()
  (incf *rot* 0.2)
  (ecs:world-tick))

(defun render ()
  (gl:clear :color-buffer)
  (gl:load-identity)
  (let* ((cam-pos (physics-pos (ecs:getcmp :physics *camera*)))
         (plr-pos (v+ (v- (physics-pos (ecs:getcmp :physics *player*)) cam-pos)
                      (vec3 320 240 0))))
    (draw-tilemap *tileset-tex* *tilemap* (vx cam-pos) (vy cam-pos))
    (draw-sprite :texture *sprite-tex*
                 :rgba '(1 1 1 1)
                 :x (vx3 plr-pos)
                 :y (vy3 plr-pos)
                 :width 64 :height 64
                 :rot (rad2deg (physics-rot (ecs:getcmp :physics *player*)))
                 :center-x 0.5 :center-y 0.5))
  (gl:flush))

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
