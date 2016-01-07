(in-package :kit.gl.tex)

(defclass texture ()
  ((id :initform (car (gl:gen-textures 1)) :initarg :id :accessor texture-id)
   (target :initform :texture-2d :initarg :target :accessor texture-target)
   (size :initarg :size :initform nil :accessor texture-size :type kit.glm:vec3)))

(defgeneric tex-parameters (tex &key &allow-other-keys)
  (:documentation "Set texture parameters for `TEX`, bound to
`TARGET`, where valid, or do nothing. `TEX` must be bound prior to calling.
Note that creating a texture causes it to become bound."))

(defmethod initialize-instance :after ((tex texture)
                                       &key
                                       (mag :linear) (min :linear)
                                       (wrap-s :repeat) (wrap-t :repeat)
                                       &allow-other-keys)
  (with-slots (id target) tex
    (when (and id target)
      (tex-bind tex)
      (tex-parameters tex :mag mag :min min :wrap-s wrap-s :wrap-t wrap-t))))

(defun active-texture (num &optional (unit :texture0))
  (let ((texture0 (cffi:foreign-enum-value '%gl:enum unit)))
    (%gl:active-texture (+ texture0 num))))

(defun tex-bind (tex &optional target)
  (let ((target (or target (slot-value tex 'target))))
    (with-slots (id) tex
      (gl:bind-texture target id))))

(defun tex-unbind (tex-or-target)
  (let ((target (etypecase tex-or-target
                  (keyword tex-or-target)
                  (texture (texture-target tex-or-target)))))
    (gl:bind-texture target 0)))

(defmethod tex-parameters ((tex texture) &key mag min wrap-s wrap-t)
  (with-slots (target) tex
    (when mag (gl:tex-parameter target :texture-mag-filter mag))
    (when min (gl:tex-parameter target :texture-min-filter min))
    (when wrap-s (gl:tex-parameter target :texture-wrap-s wrap-s))
    (when wrap-t (gl:tex-parameter target :texture-wrap-t wrap-t))))

(defmethod gl-delete-object ((tex texture))
  (with-slots (id) tex
    (gl:delete-textures (list id))))

(define-tex-fun tex-image-1d (target size)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-int-8-8-8-8) data)
  (gl:tex-image-1d target level internal-format (aref size 0) border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-1d (target)
    ((level 0) (xoffset 0) size (format :rgba) (type :unsigned-int-8-8-8-8)
     data)
  (gl:tex-sub-image-1d target level xoffset (aref size 0) format type
                       (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-2d (target size)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-int-8-8-8-8) data)
  (gl:tex-image-2d target level internal-format
                   (aref size 0) (aref size 1) border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-2d (target)
    ((level 0) (xoffset 0) (yoffset 0) size (format :rgba)
     (type :unsigned-int-8-8-8-8) data)
  (gl:tex-sub-image-2d target level xoffset yoffset
                       (aref size 0) (aref size 1)
                       format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-2d-multisample (target size)
    (samples (internal-format :rgba) fixed-sample-locations)
  (%gl:tex-image-2d-multisample target samples internal-format
                                (aref size 0) (aref size 1)
                                fixed-sample-locations))

(define-tex-fun tex-image-3d (target size)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-int-8-8-8-8) data)
  (gl:tex-image-3d target level internal-format
                   (aref size 0) (aref size 1) (aref size 2) border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-3d (target)
    ((level 0) (xoffset 0) (yoffset 0) (zoffset 0) size
     (format :rgba) (type :unsigned-int-8-8-8-8) data)
  (gl:tex-sub-image-3d target level xoffset yoffset zoffset
                       (aref size 0) (aref size 1) (aref size 2)
                       format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-3d-multisample (target size)
    (samples (internal-format :rgba) fixed-sample-locations)
  (%gl:tex-image-3d-multisample target samples internal-format
                                (aref size 0) (aref size 1) (aref size 2)
                                fixed-sample-locations))
