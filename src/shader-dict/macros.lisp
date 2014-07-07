(in-package :kit.gl.shader)

 ;; SHADER-DICTIONARIES

(defvar *shader-dictionaries* (make-hash-table))

(defun find-dictionary (name)
  (or (gethash name *shader-dictionaries*)
      (error "Shader dictionary not found: ~S" name)))

(defun define-dictionary (name programs)
  (setf (gethash name *shader-dictionaries*) programs))

(defmacro dict (name)
  `(find-dictionary ',name))

 ;; PARSE-SHADER-SOURCE

(defgeneric parse-shader-source (source shader-type shader-list)
  (:documentation "Specialize on `SHADER-SOURCE` and return a string.
`SHADER-TYPE` is the type (e.g., `:fragment-shader`).  Specializations
are predefined for *string*, *list*, and *symbol*; do not redefine
these.

`SHADER-LIST` is an optional ALIST of existing \"named\" shader
definitions in the form `(NAME . (TYPE VALUE))`.  Note that `VALUE`
may not be a string, and `PARSE-SHADER-SOURCE` must be called
recursively to resolve it."))

(defgeneric parse-shader-source-complex (key params shader-type shader-list)
  (:documentation "Much like `PARSE-SHADER-SOURCE`, except called when
the source is a list.  In this case, `KEY` is the car of that list,
`PARAMS` is the cdr, and `SHADER-TYPE` and `SHADER-LIST` are as per
`PARSE-SHADER-SOURCE`."))

(defmethod parse-shader-source ((source string) shader-type shader-list)
  (declare (ignore shader-type shader-list))
  source)

(defmethod parse-shader-source ((source list) shader-type shader-list)
  (parse-shader-source-complex (car source) (cdr source) shader-type shader-list))

(defmethod parse-shader-source ((source symbol) shader-type shader-list)
  (declare (ignore shader-type))
  (let ((shader (assoc source shader-list)))
    (if shader
        (parse-shader-source (caddr shader) (cadr shader) shader-list)
        (error "Shader not found: ~S" source))))

(defmethod parse-shader-source-complex ((key (eql :file)) params shader-type shader-list)
  (declare (ignore shader-type shader-list))
  (read-file-into-string (car params)))

 ;; DEFDICT

(defmacro defdict ((name &key shader-path) &body options)
  `(define-dictionary ',name
       (let ((shaders)
             (programs)
             (*default-pathname-defaults*
               (or ,shader-path
                   *default-pathname-defaults*)))
         (macrolet ((shader (name type value)
                      `(push (list ',name ',type ',value) shaders))
                    (program (name uniform-list &rest shaders)
                      `(push (make-instance 'program-source
                               :name ,name
                               :uniforms (list ,@uniform-list)
                               :shaders (list
                                         ,@(loop for i in shaders
                                                 collect `',(car i)
                                                 collect `(parse-shader-source ',(cadr i) ',(car i) shaders))))
                             programs)))
           ,@options
           programs))))
