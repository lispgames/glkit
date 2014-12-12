(in-package :kit.gl)

;;; Protocol for everyone

(defgeneric gl-delete-object (gl-object)
  (:documentation "Call the appropriate gl* function to free the resource
`GL-OBJECT`."))

(defmethod gl-delete-object ((gl-objects list))
  (map nil #'gl-delete-object gl-objects))

(defmethod gl-delete-object ((gl-objects vector))
  (map nil #'gl-delete-object gl-objects))

(defun gl-delete (&rest gl-objects)
  "Delete `GL-OBJECTS` by calling `GL-DELETE-OBJECT` on each."
  (map nil #'gl-delete-object gl-objects))
