(in-package #:tex-png)

(defun load-and-decode-png (filename)
  (with-open-file (input filename
                         :element-type '(unsigned-byte 8))
    (pngload:load-file filename)))

(defun png-channels (png)
  (ecase (pngload:color-type png)
    ((:truecolour :indexed-colour) 3)
    (:truecolour-alpha 4)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun png-to-texture-data (png)
  "Convert a libpng PNG object to OpenGL-compatible texture data."
  (let* ((w (pngload:width png))
         (h (pngload:height png))
         (c (png-channels png))
         (raw (pngload:data png))
         (data (make-array (list (* w h c))
                           :element-type (array-element-type raw)
                           :displaced-to raw)))
    data))

(defun png-internal-format (png)
  (let ((c (png-channels png)))
    (ecase (pngload:bit-depth png)
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
  (ecase (png-channels png)
    (1 :luminance)
    (2 :luminance-alpha)
    (3 :rgb)
    (4 :rgba)))

(defun png-data-type (png)
  (ecase (pngload:bit-depth png)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun make-texture-from-png (filename)
  (let* ((png (load-and-decode-png filename))
         (w (pngload:width png))
         (h (pngload:height png))
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
                     data)
    texture-id))
