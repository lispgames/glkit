;;; This uses VAOs and shaders.  Actual shaders are in separate files,
;;; by version.  Run the function most appropriate to your GL version:
;;;
;;; (kit.gl.test:vao-shader-330)   ; GL 3.3
;;; (kit.gl.test:vao-shader-150)   ; GL 3.2
;;; (kit.gl.test:vao-shader-120)   ; GL 2.1
;;;
;;; Note that VAOs are technically only core as of 3.0, and your
;;; driver may not support them otherwise unless you have
;;; the ARB_vertex_array_object extension.

(in-package :kit.gl.test)

;;; We'll reuse the verts from `vaos.lisp`

;;; The colors:
(defparameter *vao-colors*
  (make-array 9
   :element-type 'single-float
   :initial-contents #(1.0 0.0 0.0
                       0.0 1.0 0.0
                       0.0 0.0 1.0)))

;;; Define the layout of the VAO
(defvao vertex-color-2d ()
  (:separate ()
    (vertex :float 2)
    (color :float 3)))

(defclass vao-shader-window (kit.sdl2.test:test-window)
  ((view-matrix :initform (kit.glm:ortho-matrix -2 2 -2 2 -2 2))
   (vao :initform nil)
   (programs :initform nil)))

(defmethod initialize-instance :after ((w vao-shader-window)
                                       &key shaders &allow-other-keys)
  (setf (idle-render w) t)
  (gl:viewport 0 0 800 600)

  (with-slots (vao programs) w
    ;; Compile shaders using the dictionary name specified via :shaders
    (setf programs (compile-shader-dictionary shaders))

    ;; Make the VAO, and copy the data into it.
    (setf vao (make-instance 'vao
                :type 'vertex-color-2d
                :primitive :triangles
                :vertex-count (/ (length *vao-verts*) 2)))
    (vao-buffer-vector vao 0 (* 4 (length *vao-verts*)) *vao-verts*)
    (vao-buffer-vector vao 1 (* 4 (length *vao-colors*)) *vao-colors*)))

(defmethod render ((window vao-shader-window))
  (with-slots (view-matrix vao programs) window
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:clear :color-buffer)

    ;; Now we just tell GL to draw the contents:
    (use-program programs :vertex-color)
    (uniform-matrix programs :view-m 4 (vector view-matrix))
    (vao-draw vao)))

;; (kit.gl.test:vao-shader-330)
