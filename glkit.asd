(defpackage :glkit.asdf
  (:use #:cl #:asdf))

(in-package :glkit.asdf)

(defsystem :glkit
  :description "Various utilities for OpenGL"
  :author ("3b" "rpav")
  :license "MIT"
  :version "0.0"

  :depends-on (:alexandria :sb-cga :cl-opengl)
  :pathname "src"
  :serial t

  :components
  ((:file "package-util")
   (:file "package")

   (:module "math"
    :serial t
    :pathname "math"
    :components
    ((:file "math")
     (:file "quat")))

   (:module "shader-dict"
    :serial t
    :pathname "shader-dict"
    :components
    ((:file "shaders")))))
