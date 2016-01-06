(in-package :kit.gl.tex)

(defclass texture ()
  ((id :initform (car (gl:gen-textures 1)) :initarg :id :accessor texture-id)
   (target :initform :texture-2d :initarg :target :accessor texture-target)
   (width :initarg :width :initform nil :accessor texture-width)
   (height :initarg :height :initform nil :accessor texture-height)
   (depth :initarg :depth :initform nil :accessor texture-depth)))

(defgeneric tex-parameters (tex target &key &allow-other-keys)
  (:documentation "Set texture parameters for `TEX`, bound to
`TARGET`, where valid, or do nothing.

`TEX` must be bound prior to calling.  Do not actually bind `TEX`."))

(defmethod initialize-instance :after ((tex texture)
                                       &key
                                       (mag :nearest) (min :nearest)
                                       (wrap-s :repeat) (wrap-t :repeat)
                                       &allow-other-keys)
  (with-slots (id target) tex
    (when (and id target)
      (tex-bind tex)
      (tex-parameters tex target :mag mag :min min :wrap-s wrap-s :wrap-t wrap-t))))

(defun tex-bind (tex &optional target)
  (let ((target (or target (slot-value tex 'target))))
    (with-slots (id) tex
      (gl:bind-texture target id))))

(defun tex-unbind (tex-or-target)
  (let ((target (etypecase tex-or-target
                  (keyword tex-or-target)
                  (texture (texture-target tex-or-target)))))
    (gl:bind-texture target 0)))

(defmethod tex-parameters ((tex texture) target &key mag min wrap-s wrap-t)
  (when mag (gl:tex-parameter target :texture-mag-filter mag))
  (when min (gl:tex-parameter target :texture-min-filter min))
  (when wrap-s (gl:tex-parameter target :texture-wrap-s wrap-s))
  (when wrap-t (gl:tex-parameter target :texture-wrap-t wrap-t)))

(defmethod gl-delete-object ((tex texture))
  (with-slots (id) tex
    (gl:delete-textures (list id))))

(define-tex-fun tex-image-1d (target width)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-byte) data)
  (gl:tex-image-1d target level internal-format width border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-1d (target)
    ((level 0) (xoffset 0) width (format :rgba) (type :unsigned-byte)
     data)
  (gl:tex-sub-image-1d target level xoffset width format type
                       (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-2d (target width height)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-byte) data)
  (gl:tex-image-2d target level internal-format width height border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-2d (target)
    ((level 0) (xoffset 0) (yoffset 0) width height (format :rgba)
     (type :unsigned-byte) data)
  (gl:tex-sub-image-2d target level xoffset yoffset width height
                       format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-2d-multisample (target width height)
    (samples (internal-format :rgba) fixed-sample-locations)
  (%gl:tex-image-2d-multisample target samples internal-format width height
                                fixed-sample-locations))

(define-tex-fun tex-image-3d (target width height depth)
    ((border 0) (level 0) (internal-format :rgba) (format :rgba)
     (type :unsigned-byte) data)
  (gl:tex-image-3d target level internal-format width height depth border
                   format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-sub-image-3d (target)
    ((level 0) (xoffset 0) (yoffset 0) (zoffset 0) width height depth
     (format :rgba) (type :unsigned-byte) data)
  (gl:tex-sub-image-3d target level xoffset yoffset zoffset
                       width height depth
                       format type (or data (cffi-sys:null-pointer))))

(define-tex-fun tex-image-3d-multisample (target width height depth)
    (samples (internal-format :rgba) fixed-sample-locations)
  (%gl:tex-image-3d-multisample target samples internal-format
                                width height depth
                                fixed-sample-locations))
