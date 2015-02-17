(defpackage :glkit.asdf
  (:use #:cl #:asdf))

(in-package :glkit.asdf)

(defsystem :glkit-examples
  :description "Various utilities for OpenGL"
  :author ("rpav")
  :license "MIT"
  :version "0.0"

  :depends-on (:sdl2kit-examples :glkit)
  :pathname "examples"
  :serial t

  :components
  ((:file "vaos")
   (:file "vao-shader")
   (:file "vao-shader-150")))
