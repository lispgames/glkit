;;;
;;; Run: (kit.gl.test:vao-shader-120)
;;;
;;; This reuses everything from `vao-shader.lisp` except the shader
;;; dictionary.
;;;
;;; Use this if your card only supports GL 2.1 / GLSL 1.20 and has the
;;; ARB_vertex_array_object extension.

(in-package :kit.gl.test)

;;; Now the shaders to use it
(defdict vao-color.programs.120 ()
  (program (:vertex-color
             :uniforms (:view-m)
             :attrs ((:vertex 0)
                     (:color 1)))
           (:vertex-shader "
#version 120

uniform mat4 view_m;

in vec2 vertex;
in vec3 color;

varying vec3 f_color;

void main() {
    gl_Position = view_m * vec4(vertex, 0.0, 1.0);
    f_color = color;
}
")
           (:fragment-shader "
#version 120

in vec3 f_color;

void main() {
    gl_FragColor = vec4(f_color, 1.0);
}
")))

#-darwin
(defun vao-shader-120 ()
  (kit.sdl2:start)
  (sdl2:in-main-thread ()
    (sdl2:gl-set-attr :context-major-version 2)
    (sdl2:gl-set-attr :context-minor-version 1)
    (sdl2:gl-set-attr :context-profile-mask 1))
  (make-instance 'vao-shader-window :shaders 'vao-color.programs.120))

#+darwin
(defun vao-shader-120 ()
  (format t "This example does not work on darwin. Try running (kit.gl.test:vao-shader-410)."))

;; (kit.gl.test:vao-shader-120)
