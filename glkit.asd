(defpackage :glkit.asdf
  (:use #:cl #:asdf))

(in-package :glkit.asdf)

#+(or allegro ccl cmu ecl lispworks sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :glkit-sv *features*))

(defsystem :glkit
  :description "Various utilities for OpenGL"
  :author ("rpav")
  :license "MIT"
  :version "0.0"

  :depends-on (:alexandria :defpackage-plus :sb-cga :mathkit
               :cl-opengl #+glkit-sv :static-vectors)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "protocol")

   (:module "shader-dict"
    :serial t
    :pathname "shader-dict"
    :components
    ((:file "shaders")
     (:file "uniforms")
     (:file "macros")))

   (:module "vao"
    :serial t
    :pathname "vao"
    :components
    ((:file "vao")))

   (:module "tex"
    :serial t
    :pathname "tex"
    :components
    ((:file "util")
     (:file "texture")
     (:file "fbo")))))
