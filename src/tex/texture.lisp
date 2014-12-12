(in-package :kit.gl.tex)

(defclass texture ()
  ((id :initform (car (gl:gen-textures 1)) :initarg :id)
   (target :initform :texture-2d :initarg :target)))

(defmethod initialize-instance :after ((tex texture)
                                       &key
                                       (mag :nearest) (min :nearest)
                                       (wrap-s :repeat) (wrap-t :repeat)
                                       &allow-other-keys)
  (with-slots (id target) tex
    (gl:bind-texture target id)
    (gl:tex-parameter target :texture-mag-filter mag)
    (gl:tex-parameter target :texture-min-filter min)
    (gl:tex-parameter target :texture-wrap-s wrap-s)
    (gl:tex-parameter target :texture-wrap-t wrap-t)))
