;;; This is based on the sdl2kit example; you must load that first!
;;;
;;; Run: (kit.sdl2.test:vaos)
;;;

(in-package :kit.gl.test)

;;; Using a specific :element-type is necessary for VAO-BUFFER-VECTOR
(defparameter *vao-verts*
  (make-array 6
   :element-type 'single-float
   :initial-contents #(0.0 0.0
                       -1.0 -1.0
                       1.0 1.0)))

;;; Define the layout of the VAO
(defvao vertex-2d ()
  (:separate ()
    (vertex :float 2)))

(defclass vao-window (kit.sdl2.test:test-window)
  ((vao :initform nil)))

(defmethod initialize-instance :after ((w vao-window) &key &allow-other-keys)
  (with-slots (vao) w
    ;; Make the VAO, and copy the data into it.
    (setf vao (make-instance 'vao :type 'vertex-2d))
    (vao-buffer-vector vao 0 (* 4 (length *vao-verts*)) *vao-verts*)))

(defmethod render ((window vao-window))
  (with-slots (vao rotation) window
    (gl:load-identity)
    (gl:rotate rotation 0 0 1)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:clear :color-buffer)
    ;; Now we just tell GL to draw the contents:
    (vao-draw vao)))

(defun vaos ()
  (kit.sdl2:start)
  (make-instance 'vao-window))
