(in-package :kit.gl.shader)

;; (defvar *shader-dict*
;;   '((:solid
;;      (:uniforms ((x "x") (y "y")))
;;      (:shaders
;;       :vertex-shader "..."
;;       :fragment-shader "..."))))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun compile-and-link-program (&rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (let (compiled-shaders)
    (loop for type in shaders by #'cddr
          for text in (cdr shaders) by #'cddr
          do (let ((shader (gl:create-shader type)))
               (when-let ((log (compile-and-check-shader shader text)))
                 (format *error-output* "Compile Log for ~A:~%~A~%" type log))
               (push shader compiled-shaders)))
    (let ((program (gl:create-program)))
      (if (= 0 program)
          (progn
            (loop for shader in compiled-shaders
                  do (gl:delete-shader shader))
            (error "Error creating program"))
          (progn
            (loop for shader in compiled-shaders
                  do (gl:attach-shader program shader))
            (gl:link-program program)
            (let ((log (gl:get-program-info-log program)))
              (unless (string= "" log)
                (format *error-output* "Link Log:~%~A~%" log)))
            (loop for shader in compiled-shaders
                  do (gl:detach-shader program shader)
                     (gl:delete-shader shader))))
      program)))

(defclass program-source ()
  ((name :initarg :name)
   (shaders :initarg :shaders)
   (uniforms :initarg :uniforms)))

(defmethod print-object ((o program-source) stream)
  (with-slots (name) o
    (print-unreadable-object (o stream :type t)
      (format stream "~S" name))))

(defclass program ()
  ((name :initform nil :initarg :name)
   (id :initform nil)
   (uniforms :initform (make-hash-table :test 'equal))))

(defclass shader-dictionary ()
  ((programs :initform (make-hash-table))
   (active-program :initform nil)))

(defun process-source (source program)
  (with-slots (shaders) source
    (let ((p (apply #'compile-and-link-program shaders)))
      (gl:use-program p)
      (with-slots (id  uniforms) program
        (setf id p)
        (with-slots ((source-uniforms uniforms)) source
          (loop for uniform in source-uniforms
                as symbol = (if (symbolp uniform) uniform (car uniform))
                as name = (if (or (symbolp uniform)
                                  (not (cadr uniform)))
                              (symbol-to-uniform uniform)
                              (cadr uniform))
                as loc = (gl:get-uniform-location id name)
                do (setf (gethash symbol uniforms) loc)))))))

(defun symbol-to-uniform (symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defun find-program (dictionary name)
  (with-slots (programs) dictionary
    (gethash name programs)))

(defun find-uniform (dictionary program name)
  (with-slots (uniforms) (find-program dictionary program)
    (gethash name uniforms)))

(defun compile-shader-dictionary (sources)
  "Input is a list of PROGRAM-SOURCE objects.  Returns a new
SHADER-DICTIONARY object.  This must be called with a valid, active
GL-CONTEXT.  The result is only valid while that GL-CONTEXT is valid."
  (let ((sd (make-instance 'shader-dictionary)))
    (with-slots (programs) sd
      (loop for program-source in sources
            as name = (slot-value program-source 'name)
            as program = (make-instance 'program :name name)
            do (setf (gethash name programs) program)
               (process-source program-source program)))
    sd))

(defun use-program (dict program)
  "Set program named `PROGRAM` in `DICT` as the active program."
  (with-slots (active-program) dict
    (let ((p (find-program dict program)))
      (with-slots (id) p
        (setf active-program p)
        (gl:use-program id)))))