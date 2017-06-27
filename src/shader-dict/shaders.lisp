(in-package :kit.gl.shader)

 ;; Classes

(defclass shader-dictionary-definition ()
  ((name :initarg :name)
   (path :initform *default-pathname-defaults* :initarg :path)
   (shaders :initarg :shaders)
   (programs :initarg :programs)))

(defclass program-source ()
  ((name :initarg :name)
   (shaders :initarg :shaders)
   (vertex-attributes :initarg :attrs :initform nil)
   (uniform-style :initarg :uniform-style)
   (uniforms :initarg :uniforms)))

(defclass program ()
  ((name :initform nil :initarg :name)
   (id :initform nil)
   (uniforms :initform (make-hash-table :test 'equal))))

(defclass compiled-shader-dictionary ()
  ((programs :initform (make-hash-table))
   (active-program :initform nil)))

 ;; Functions

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun compile-and-link-program (attrs &rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (let (compiled-shaders)
    (loop for type in shaders by #'cddr
          for text in (cdr shaders) by #'cddr
          do (let ((shader (gl:create-shader type)))
               (when-let ((log (compile-and-check-shader shader text)))
                 (error "Compile Log for ~A:~%~A~%" type log))
               (push shader compiled-shaders)))
    (let ((program (gl:create-program)))
      (if (= 0 program)
          (progn
            (loop for shader in compiled-shaders
                  do (gl:delete-shader shader))
            (error "Error creating program: ~A"
                   (gl:get-error)))
          (progn
            (loop for shader in compiled-shaders
                  do (gl:attach-shader program shader))
            (loop for attr in attrs
                  as name = (if (symbolp (car attr))
                                (string-downcase (string (car attr)))
                                (car attr))
                  as index = (cadr attr)
                  do (handler-case
                         (gl:bind-attrib-location program index name)
                       (gl:opengl-error (e)
                         (error "Error binding attribute ~S:~%~A"
                                name e))))
            (gl:link-program program)
            (unless (gl:get-program program :link-status)
              (error "Link Log:~%~A~%" (gl:get-program-info-log program)))
            (loop for shader in compiled-shaders
                  do (gl:detach-shader program shader)
                     (gl:delete-shader shader))))
      program)))

(defmethod print-object ((o program-source) stream)
  (with-slots (name) o
    (print-unreadable-object (o stream :type t)
      (format stream "~S" name))))

(defun process-source (dict source program)
  (let ((shaders
          (let ((*default-pathname-defaults*
                  (slot-value dict 'path)))
            (loop with other-shaders = (slot-value dict 'shaders)
                  for shader-source in (slot-value source 'shaders)
                  as type = (car shader-source)
                  collect type
                  collect
                  (parse-shader-source (cadr shader-source)
                                       type
                                       other-shaders)))))
    (let ((p (apply #'compile-and-link-program
                    (slot-value source 'vertex-attributes) shaders)))
      (gl:use-program p)
      (with-slots (id  uniforms) program
        (setf id p)
        (with-slots (uniform-style (source-uniforms uniforms)) source
          (loop for uniform in source-uniforms
                as symbol = (if (symbolp uniform) uniform (car uniform))
                as name = (if (or (symbolp uniform)
                                  (not (cadr uniform)))
                              (symbol-to-uniform uniform-style uniform)
                              (cadr uniform))
                as loc = (gl:get-uniform-location id name)
                do (setf (gethash symbol uniforms) loc)))))))

(defmethod symbol-to-uniform ((uniform-style (eql :underscore)) symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defmethod symbol-to-uniform ((uniform-style (eql :camel-case)) symbol)
  (let ((result (string-downcase (symbol-name symbol))))
    (loop :for char :across result
          :for i :from 0
          :when (char= char #\-)
            :do (setf (elt result (1+ i)) (char-upcase (elt result (1+ i)))
                      result (remove char result :count 1))
                (decf i))
    result))

(defun find-program (dictionary name)
  (with-slots (programs) dictionary
    (gethash name programs)))

(defun find-uniform (dictionary program name)
  (with-slots (id uniforms) (find-program dictionary program)
    (if (stringp name)
        (gl:get-uniform-location id name)
        (gethash name uniforms))))

(defgeneric compile-shader-dictionary (source))

(defmethod compile-shader-dictionary ((sources shader-dictionary-definition))
  "Input is a list of PROGRAM-SOURCE objects.  Returns a new
COMPILED-SHADER-DICTIONARY object.  This must be called with a valid, active
GL-CONTEXT.  The result is only valid while that GL-CONTEXT is valid."
  (let ((sd (make-instance 'compiled-shader-dictionary)))
    (with-slots (path shaders (source programs)) sources
      (with-slots ((compiled-programs programs)) sd
        (loop for program-source in source
              as name = (slot-value program-source 'name)
              as program = (make-instance 'program :name name)
              do (setf (gethash name compiled-programs) program)
                 (process-source sources program-source program))))
    (gl:use-program 0)
    sd))

(defmethod compile-shader-dictionary ((source symbol))
  (compile-shader-dictionary (find-dictionary source)))

(defun use-program (dict program)
  "Set program named `PROGRAM` in `DICT` as the active program.
`PROGRAM` may be 0 or NIL, in which case, this has the same effect as
calling (gl:use-program 0).  In this case, it is valid to pass `NIL`
for `DICT`."
  (with-slots (active-program) dict
    (if (or (null program)
            (and (numberp program) (= 0 program)))
        (progn
          (when dict
            (setf active-program nil))
          (gl:use-program 0))
        (let ((p (find-program dict program)))
          (unless p
            (error "Program not found in dictionary: ~S" program))
          (with-slots (id) p
            (setf active-program p)
            (gl:use-program id))))))

(defmethod gl-delete-object ((d compiled-shader-dictionary))
  (with-slots (programs) d
    (apply #'gl-delete (alexandria:hash-table-values programs))))

(defmethod gl-delete-object ((p program))
  (with-slots (id) p
    (%gl:delete-program id)))
