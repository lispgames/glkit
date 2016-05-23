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
   #:uniform-matrix-1-sv

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
   #:vao-draw #:vao-draw-instanced
   #:vao-indexed #:vao-indexed-draw))

 ;; KIT.GLM

(defpackage+ :kit.glm
  (:use #:cl)
  (:inherit #:sb-cga #:kit.math))

 ;; KIT.GL.TEX

(defpackage+ :kit.gl.tex
  (:use #:cl #:kit.gl)
  (:export-only

   ;; TEXTURE
   #:texture #:texture-id
   #:texture-size
   #:texture-width #:texture-height #:texture-depth
   #:texture-target
   #:tex-bind #:tex-unbind #:tex-parameters
   #:tex-image-1d #:tex-image-2d #:tex-image-3d

   #:active-texture

   ;; FBO
   #:framebuffer #:framebuffer-id #:framebuffer-target
   #:fbo-bind #:fbo-unbind
   #:fbo-texture #:fbo-texture-1d #:fbo-texture-2d #:fbo-texture-3d
   #:fbo-renderbuffer

   ;; RENDERBUFFER
   #:renderbuffer #:renderbuffer-id
   ))
