(in-package :kit.gl)

 ;; KIT.GL.SHADER

(ensure-package "KIT.GL.SHADER" :use '(#:cl #:alexandria))

(ensure-export-only
 '(#:compile-and-check-shader
   #:compile-and-link-program
   #:program #:shader-dictionary
   #:find-program #:find-uniform
   #:compile-shader-dictionary
   #:use-program
   #:uniformi #:uniformf #:uniformfv #:uniform-matrix))

 ;; KIT.GLM

(ensure-package "KIT.GLM" :use '(#:cl #:sb-cga))

;;; Re-export SB-CGA and mathkit symbols so they can be accessed from
;;; one place
(ensure-export (package-external-symbols :sb-cga)
               :kit.glm)
(ensure-export (package-external-symbols :kit.math)
               :kit.glm)
