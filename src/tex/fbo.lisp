(in-package :kit.gl.tex)

 ;; FRAMEBUFFER

(defclass framebuffer ()
  ((id :initform (car (gl:gen-framebuffers 1)) :initarg :id)))

(defmethod initialize-instance :after ((fbo framebuffer)
                                       &key (target :framebuffer)
                                       &allow-other-keys)
  (with-slots (id) fbo
    (%gl:bind-framebuffer target id)))

(defmethod gl-delete-object ((fbo framebuffer))
  (with-slots (id) fbo
    (gl:delete-framebuffers (list id))))

 ;; RENDERBUFFER

(defclass renderbuffer ()
  ((id :initform (car (gl:gen-renderbuffers 1)) :initarg :id)))

(defmethod initialize-instance :after ((rbo renderbuffer)
                                       &key (width 1024) (height 1024) (samples 0) (internal-format :rgba8)
                                       &allow-other-keys)
  (with-slots (id) rbo
    (gl:bind-renderbuffer :renderbuffer id)
    (%gl:renderbuffer-storage-multisample
     :renderbuffer samples internal-format width height)))

(defmethod gl-delete-object ((rbo renderbuffer))
  (with-slots (id) rbo
    (gl:delete-renderbuffers (list id))))
