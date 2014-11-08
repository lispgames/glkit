(in-package :defpackage-plus-1)

(defpackage+ :kit.gl)

 ;; KIT.GL.SHADER

(defpackage+ :kit.gl.shader
  (:use #:cl #:alexandria)
  (:export-only

   #:compile-and-check-shader
   #:compile-and-link-program
   #:program #:shader-dictionary
   #:find-program #:find-uniform
   #:compile-shader-dictionary
   #:use-program
   #:uniformi #:uniformf #:uniformfv #:uniform-matrix

   #:defdict #:dict #:find-dictionary #:define-dictionary
   #:program #:shader

   #:parse-shader-source #:parse-shader-source-complex))

 ;; KIT.GL.VAO

(defpackage+ :kit.gl.vao
  (:use #:cl)
  (:export-only
   #:defvao
   #:vao #:vao-buffer-data #:vao-buffer-sub-data
   #:vao-bind #:vao-unbind
   #:vao-draw))

 ;; KIT.GLM

(defpackage+ :kit.glm
  (:use #:cl)
  (:inherit #:sb-cga #:kit.math))

