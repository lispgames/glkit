(defpackage :glkit.asdf
  (:use #:cl #:asdf))

(in-package :glkit.asdf)

(defsystem :glkit
  :description "Various utilities for OpenGL"
  :author ("rpav")
  :license "MIT"
  :version "0.0"

  :depends-on (:alexandria :sb-cga :mathkit :cl-opengl)
  :pathname "src"
  :serial t

  :components
  ((:file "package-util")
   (:file "package")

   (:module "shader-dict"
    :serial t
    :pathname "shader-dict"
    :components
    ((:file "shaders")
     (:file "uniforms")
     (:file "macros")))))
