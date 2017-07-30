(in-package :kit.gl.vao)

(defvar *vao-declarations* (make-hash-table))
(defvar *vao* nil)

(defclass vertex-attribute ()
  ((name :initarg :name :reader vertex-attribute-name)
   (type :initarg :type)
   (out-type :initarg :out-type)
   (count :initarg :count)
   (normalizep :initarg :normalizep)))

(defclass vertex-attribute-group ()
  ((divisor :initform 0 :initarg :divisor)
   (attributes :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass vertex-interleave-group (vertex-attribute-group) ())

(defclass vertex-block-group (vertex-attribute-group) ())

(defclass vertex-separate-group (vertex-attribute-group) ())

(defmethod print-object ((o vertex-attribute-group) stream)
  (with-slots (attributes) o
    (print-unreadable-object (o stream :type t)
      (format stream "~S" (map 'list #'vertex-attribute-name attributes)))))

(defclass vao ()
  ((type :type vao-declaration)
   (id :initform (gl:gen-vertex-array))
   (vbos)
   (vertex-count :initform nil :initarg :vertex-count)
   (pointers :initform nil)
   (primitive :initarg :primitive :initform nil)))

(defclass vao-indexed (vao)
  ((index :initarg :index)))

(defmethod initialize-instance :after ((vao vao) &key type &allow-other-keys)
  (vao-bind vao)
  (with-slots ((vao-type type) id vbos vertex-count) vao
    (if type
        (setf vao-type (vao-find type))
        (error "No :TYPE specified for VAO."))
    (let ((vbo-count (vao-vbo-count vao-type)))
      (setf vbos (map 'vector #'identity (gl:gen-buffers vbo-count))))
    (with-slots (groups) vao-type
      (loop :for group :across groups
            :for vbo-offset = 0 :then (+ vbo-offset vbo-count)
            :for vbo-count = (vao-vbo-count group)
            :for vbo-subset = (make-array vbo-count :displaced-to vbos
                                                    :displaced-index-offset vbo-offset)
            :for attr-offset = 0 :then (+ attr-offset attr-count)
            :for attr-count = (vao-attr-count group)
            :do (loop :for i :below (vao-attr-count group)
                      :do (%gl:enable-vertex-attrib-array (+ i attr-offset)))
                (vao-set-pointers group attr-offset vertex-count vbo-subset)))))

(defclass vao-declaration ()
  ((attr-index :initform (make-hash-table))
   (attr-count :initform 0)
   (groups :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defun vao-find (name)
  (gethash name *vao-declarations*))

(defun attribute-set-pointer (attr index stride offset divisor)
  (with-slots (type out-type count normalizep) attr
    (ecase out-type
      ((:byte :short :int :unsigned-int)
       (%gl:vertex-attrib-ipointer index count type stride offset))
      ((:float :half-float)
       (%gl:vertex-attrib-pointer index count type
                                  (if normalizep 1 0)
                                  stride offset))
      (:double
       (%gl:vertex-attrib-lpointer index count type stride offset)))
    (when divisor
      (%gl:vertex-attrib-divisor index divisor))))

(defun vao-decl-add-index (decl attr)
  (with-slots (attr-index attr-count) decl
    (setf (gethash (vertex-attribute-name attr) attr-index) attr-count)
    (incf attr-count)))

(defgeneric vao-add (vao object)
  (:method (vao object)
    (error "You may not add ~%  ~S~% to~%  ~S" object vao)))

(defmethod vao-add ((vao vao-declaration) (group vertex-attribute-group))
  (with-slots (groups) vao
    (vector-push-extend group groups)))

(defmethod vao-add ((group vertex-attribute-group) (attr vertex-attribute))
  (with-slots (attributes) group
    (vector-push-extend attr attributes)))

(defmethod vao-add ((group vertex-block-group) (ig vertex-interleave-group))
  (with-slots (attributes) group
    (vector-push-extend ig attributes)))

(defmethod vao-add ((group vertex-attribute-group) (sg vertex-separate-group))
  (with-slots (attributes) group
    (vector-push-extend sg attributes)))

(defgeneric vao-attr-count (object)
  (:documentation "Number of slots for `VAO`.")
  (:method (object) 1))

(defmethod vao-attr-count ((object vao))
  (with-slots (type) object
    (vao-attr-count type)))

(defmethod vao-attr-count ((object vertex-attribute-group))
  (with-slots (attributes) object
    (reduce #'+ (map 'list #'vao-attr-count attributes))))

(defmethod vao-attr-count ((object vao-declaration))
  (with-slots (groups) object
    (reduce #'+ (map 'list #'vao-attr-count groups))))

(defgeneric vao-vbo-count (object)
  (:documentation "Number of VBOs allocated by `GROUP`.")
  (:method (object) 1))

(defmethod vao-vbo-count ((object vertex-separate-group))
  (with-slots (attributes) object
    (reduce #'+ (map 'list #'vao-vbo-count attributes))))

(defmethod vao-vbo-count ((object vao-declaration))
  (with-slots (groups) object
    (reduce #'+ (map 'list #'vao-vbo-count groups))))

(defgeneric attribute-size (attr)
  (:documentation "Total size in bytes of `ATTR`.")
  (:method (attr)
    (ecase attr
      ((:byte :unsigned-byte) 1)
      ((:short :half-float) 2)
      ((:float :int :unsigned-int) 4)
      (:double 8))))

(defmethod attribute-size ((attr vertex-attribute))
  (with-slots (type count) attr
   (* (attribute-size type) count)))

(defmethod attribute-size ((group vertex-attribute-group))
  (with-slots (attributes) group
    (loop :for attr :across attributes
          :sum (attribute-size attr))))

(defgeneric vao-set-pointers (group starting-index total-vertices vbos))

(defmethod vao-set-pointers ((group vertex-separate-group) starting-index
                             vertex-count vbos)
  (declare (ignore vertex-count))
  (with-slots (attributes divisor) group
    (loop :for attr :across attributes
          :for attr-index :from starting-index
          :for vbo-index :from 0
          :do (gl:bind-buffer :array-buffer (aref vbos vbo-index))
              (attribute-set-pointer attr attr-index 0 0 divisor))))

(defmethod vao-set-pointers ((group vertex-interleave-group) starting-index
                             vertex-count vbos)
  (declare (ignore vertex-count))
  (with-slots (attributes divisor) group
    (%gl:bind-buffer :array-buffer (aref vbos 0))
    (loop :with stride = (attribute-size group)
          :with offset = 0
          :for attr :across attributes
          :for i :from starting-index
          :do (attribute-set-pointer attr i stride offset divisor)
              (incf offset (attribute-size attr)))))

(defmethod vao-set-pointers ((group vertex-block-group) starting-index
                             vertex-count vbos)
  (error "Implement VAO-SET-POINTERS for block groups"))

(defun vao-parse (list)
  (if (listp (cadr list))
      (vao-parse-group
       (vao-parse-make-group (car list) (cadr list))
       (cddr list))
      (apply #'vao-parse-decl list)))

(defun vao-parse-decl (name type count &key out-type normalizep)
  (let ((attr (make-instance 'vertex-attribute
                :name name
                :type type
                :count count
                :out-type (or out-type type)
                :normalizep normalizep)))
    (vao-decl-add-index *vao* attr)
    attr))

(defgeneric vao-parse-make-group (type options))

(defmethod vao-parse-make-group ((type (eql :interleave)) options)
  (apply #'make-instance 'vertex-interleave-group options))

(defmethod vao-parse-make-group ((type (eql :blocks)) options)
  (apply #'make-instance 'vertex-block-group options))

(defmethod vao-parse-make-group ((type (eql :separate)) options)
  (apply #'make-instance 'vertex-separate-group options))

(defgeneric vao-parse-group (group body))

(defun vao-parse-group (group body)
  (loop :for i :in body
        :do (vao-add group (vao-parse i)))
  group)

(defmacro defvao (name options &body groups)
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((*vao* (make-instance 'vao-declaration)))
       (map 'nil (lambda (x) (vao-add *vao* (vao-parse x))) ',groups)
       (setf (gethash ',name *vao-declarations*) *vao*))))

(defun vao-bind (vao)
  (with-slots (id) vao
    (%gl:bind-vertex-array id)))

(defun vao-unbind ()
  (%gl:bind-vertex-array 0))

(defun guess-buffer-size (array)
  (let* ((count (length array))
         (type-size
           (etypecase array
             ((simple-array single-float *) 4)
             ((simple-array double-float *) 8)
             ((simple-array (signed-byte 8) *) 1)
             ((simple-array (unsigned-byte 8) *) 1)
             ((simple-array (signed-byte 16) *) 2)
             ((simple-array (unsigned-byte 16) *) 2)
             ((simple-array (signed-byte 32) *) 4)
             ((simple-array (unsigned-byte 32) *) 4))))
    (* count type-size)))

(defmacro with-vao-buffer ((ptr size vector) &body body)
  `(let* ((sv (static-vectors:make-static-vector
               (length ,vector)
               :element-type (array-element-type ,vector)
               :initial-contents ,vector))
          (,size (or ,size (guess-buffer-size ,vector)))
          (,ptr (static-vectors:static-vector-pointer sv)))
     ,@body
     (static-vectors:free-static-vector sv)))

(defun vao-buffer-data (vao vbo byte-size pointer &optional (usage :dynamic-draw))
  (with-slots (vbos) vao
    (%gl:bind-buffer :array-buffer (aref vbos vbo))
    (%gl:buffer-data :array-buffer byte-size pointer usage)))

(defun vao-buffer-vector (vao vbo vector &key byte-size (usage :dynamic-draw))
  #+glkit-sv
  (with-vao-buffer (ptr byte-size vector)
    (vao-buffer-data vao vbo byte-size ptr usage))
  #-glkit-sv
  (error "STATIC-VECTORS not supported by your implementation."))

(defun vao-buffer-sub-data (vao vbo offset byte-size pointer)
  (with-slots (vbos) vao
    (%gl:bind-buffer :array-buffer (aref vbos vbo))
    (%gl:buffer-sub-data :array-buffer offset byte-size pointer)))

(defun vao-buffer-sub-vector (vao vbo offset vector &key byte-size)
  #+glkit-sv
  (with-vao-buffer (ptr byte-size vector)
    (vao-buffer-sub-data vao vbo offset byte-size ptr))
  #-glkit-sv
  (error "STATIC-VECTORS not supported by your implementation."))

(defun vao-draw (vao &key primitive (first 0) count)
  (with-slots ((prim primitive) vertex-count) vao
    (vao-bind vao)
    (%gl:draw-arrays (or primitive prim) first (or count vertex-count))))

(defun vao-draw-instanced (vao prim-count &key primitive (first 0) count)
  (with-slots ((prim primitive) vertex-count) vao
    (vao-bind vao)
    (%gl:draw-arrays-instanced (or primitive prim) first (or count vertex-count) prim-count)))

(defun vao-draw-elements (vao &key primitive index count type)
  (with-slots ((prim primitive) (ind index) vertex-count) vao
    (vao-bind vao)
    (%gl:draw-elements (or primitive prim)
                       (or count vertex-count)
                       type
                       (or index ind))))

(defun vao-draw-elements-instanced (vao prim-count &key primitive index count type)
  (with-slots ((prim primitive) (ind index) vertex-count) vao
    (vao-bind vao)
    (%gl:draw-elements-instanced (or primitive prim)
                                 (or count vertex-count)
                                 type
                                 (or index ind)
                                 prim-count)))

(defmethod gl-delete-object ((vao vao))
  (with-slots (vbos id) vao
    (gl:delete-buffers vbos)
    (gl:delete-vertex-arrays (list id))))
