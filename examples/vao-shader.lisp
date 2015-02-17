;;; This is based on the sdl2kit example; you must load that first!
;;;
;;; Run: (kit.gl.test:vao-shader)
;;;

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

;;; Now the shaders to use it
(defdict vao-color.programs ()
  (program :vertex-color (:view-m)
           (:vertex-shader "
#version 330

uniform mat4 view_m;

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec3 color;

smooth out vec3 f_color;

void main() {
    gl_Position = view_m * vec4(vertex, 0.0, 1.0);
    f_color = color;
}
")
           (:fragment-shader "
#version 330

in vec3 f_color;
out vec4 f_out;

void main() {
    f_out = vec4(f_color, 1.0);
}
")))

(defclass vao-shader-window (kit.sdl2.test:test-window)
  ((view-matrix :initform (kit.glm:ortho-matrix -2 2 -2 2 -2 2))
   (vao :initform nil)
   (programs :initform nil)))

(defmethod initialize-instance :after ((w vao-shader-window)
                                       &key (shaders 'vao-color.programs) &allow-other-keys)
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

(defun vao-shader ()
  (kit.sdl2:start)
  (make-instance 'vao-shader-window))

;; (kit.gl.test:vao-shader)
