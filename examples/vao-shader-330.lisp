;;;
;;; Run: (kit.gl.test:vao-shader-330)
;;;
;;; Use this if your card supports GL 3.3 / GLSL 3.30

(in-package :kit.gl.test)

(defdict vao-color.programs.330 ()
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

smooth in vec3 f_color;
out vec4 f_out;

void main() {
    f_out = vec4(f_color, 1.0);
}
")))

(defun vao-shader-330 ()
  (kit.sdl2:start)
  (sdl2:in-main-thread ()
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask 1))
  (make-instance 'vao-shader-window :shaders 'vao-color.programs.330))

;; (kit.gl.test:vao-shader-330)
