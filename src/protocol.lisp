(in-package :kit.gl)

;;; Protocol for everyone

(defgeneric gl-delete-object (gl-object)
  (:documentation "Call the appropriate gl* function to free the resource
`GL-OBJECT`."))

(defun gl-delete (&rest gl-objects)
  "Delete `GL-OBJECTS` by calling `GL-DELETE-OBJECT` on each."
  (map nil #'gl-delete-object gl-objects))
