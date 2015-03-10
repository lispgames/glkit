;;;
;;; Run: (kit.gl.test:vao-shader-150)
;;;
;;; This reuses everything from `vao-shader.lisp` except the shader
;;; dictionary.
;;;
;;; Use this if your card only supports GL 3.2 / GLSL 1.50

(in-package :kit.gl.test)

;;; Now the shaders to use it
(defdict vao-color.programs.150 ()
  (program (:vertex-color
             :uniforms (:view-m)
             :attrs ((:vertex 0)
                     (:color 1)))
           (:vertex-shader "
#version 150

uniform mat4 view_m;

in vec2 vertex;
in vec3 color;

smooth out vec3 f_color;

void main() {
    gl_Position = view_m * vec4(vertex, 0.0, 1.0);
    f_color = color;
}
")
           (:fragment-shader "
#version 150

in vec3 f_color;
out vec4 f_out;

void main() {
    f_out = vec4(f_color, 1.0);
}
")))

(defun vao-shader-150 ()
  (kit.sdl2:start)
  (sdl2:in-main-thread ()
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 2))
  (make-instance 'vao-shader-window :shaders 'vao-color.programs.150))

;; (kit.gl.test:vao-shader-150)
