(in-package :kit.gl.tex)

 ;; FRAMEBUFFER

(defclass framebuffer ()
  ((id :initform (car (gl:gen-framebuffers 1)) :initarg :id :accessor framebuffer-id)
   (target :initform :framebuffer :initarg :target :accessor framebuffer-target)))

(defmethod initialize-instance :after ((fbo framebuffer) &key &allow-other-keys)
  (with-slots (id target) fbo
    (when (and id target) (fbo-bind fbo))))

(defmethod gl-delete-object ((fbo framebuffer))
  (with-slots (id) fbo
    (gl:delete-framebuffers (list id))))

(defun fbo-bind (fbo &optional target)
  (with-slots (id) fbo
    (with-slot-values-override (target) fbo
      (%gl:bind-framebuffer target id))))

(defun fbo-unbind (fbo-or-target)
  (let ((target (etypecase fbo-or-target
                  (keyword fbo-or-target)
                  (framebuffer (framebuffer-target fbo-or-target)))))
    (%gl:bind-framebuffer target 0)))

(defun fbo-texture (fbo attachment texture &key target (level 0))
  (let ((texture (etypecase texture
                   (texture (texture-id texture))
                   (integer texture))))
    (with-slot-values-override (target) fbo
      (%gl:framebuffer-texture target attachment texture level))))

(defun fbo-texture-1d (fbo attachment texture &key textarget target (level 0))
  (let ((texture-id (etypecase texture
                      (texture (texture-id texture))
                      (integer texture))))
    (with-slot-values-override (target) fbo
      (with-slot-values-override ((textarget target)) texture
        (%gl:framebuffer-texture-1d target attachment textarget texture-id level)))))

(defun fbo-texture-2d (fbo attachment texture &key textarget target (level 0))
  (let ((texture-id (etypecase texture
                      (texture (texture-id texture))
                      (integer texture))))
    (with-slot-values-override (target) fbo
      (with-slot-values-override ((textarget target)) texture
        (%gl:framebuffer-texture-2d target attachment textarget texture-id level)))))

(defun fbo-texture-3d (fbo attachment texture layer
                       &key textarget target (level 0))
  (let ((texture-id (etypecase texture
                      (texture (texture-id texture))
                      (integer texture))))
    (with-slot-values-override (target) fbo
      (with-slot-values-override ((textarget target)) texture
        (%gl:framebuffer-texture-3d target attachment textarget texture-id
                                    level layer)))))

(defun fbo-renderbuffer (fbo attachment renderbuffer &key target)
  (with-slots ((renderbuffer-id id)) renderbuffer
    (with-slot-values-override (target) fbo
      (%gl:framebuffer-renderbuffer target attachment :renderbuffer renderbuffer-id))))

 ;; RENDERBUFFER

(defclass renderbuffer ()
  ((id :initform (car (gl:gen-renderbuffers 1)) :initarg :id :accessor renderbuffer-id)))

(defmethod initialize-instance :after ((rbo renderbuffer)
                                       &key (width 1024) (height 1024) (samples 0)
                                       (internal-format :rgba)
                                       &allow-other-keys)
  (with-slots (id) rbo
    (gl:bind-renderbuffer :renderbuffer id)
    (%gl:renderbuffer-storage-multisample
     :renderbuffer samples internal-format width height)))

(defmethod gl-delete-object ((rbo renderbuffer))
  (with-slots (id) rbo
    (gl:delete-renderbuffers (list id))))
