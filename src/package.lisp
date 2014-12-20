(in-package :defpackage-plus-1)

(defpackage+ :kit.gl
  (:use #:cl)
  (:export-only
   ;; Utility
   #:gl-delete

   ;; Protocol
   #:gl-delete-object))

 ;; KIT.GL.SHADER

(defpackage+ :kit.gl.shader
  (:use #:cl #:alexandria #:kit.gl)
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
  (:use #:cl #:kit.gl)
  (:export-only
   #:defvao
   #:vao #:vao-buffer-data #:vao-buffer-sub-data
   #:vao-buffer-vector #:vao-buffer-sub-vector
   #:vao-bind #:vao-unbind
   #:vao-draw))

 ;; KIT.GLM

(defpackage+ :kit.glm
  (:use #:cl)
  (:inherit #:sb-cga #:kit.math))

 ;; KIT.GL.TEX

(defpackage+ :kit.gl.tex
  (:use #:cl #:kit.gl)
  (:export-only
   ))

 ;; KIT.GL.TEST

(defpackage+ :kit.gl.test
  (:use #:cl #:alexandria #:kit.gl #:kit.gl.vao)
  (:export :vaos))
