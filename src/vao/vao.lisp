(in-package :kit.gl.vao)

(defvar *vao-declarations* (make-hash-table))

 ;; GFs

(defgeneric vao-add (vao object))

(defgeneric vao-attr-count (vao)
  (:documentation "Number of slots for `VAO`."))

(defgeneric vao-vbo-count (group)
  (:documentation "Number of VBOs allocated by `GROUP`."))

(defgeneric vao-set-pointers (group starting-index total-vertices vbos)
  (:documentation "Use glVertexAttribPointer and related to define the
attributes.  `VBOS` is a vector of `VAO-VBO-COUNT` VBOs.  It is
necessary to bind each of these as appropriate.  It is not necessary
to call `gl:enable-vertex-attrib-array`.

`STARTING-INDEX` is the starting vertex attribute index for this group.

`TOTAL-VERTICES` is the known vertex count, or `NIL` if it is
unknown."))

(defgeneric attribute-size (attr)
  (:documentation "Total size in bytes of `ATTR`."))

(defmethod attribute-size ((attr symbol))
  (ecase attr
    (:byte 1)
    ((:short :half-float) 2)
    ((:float :int :unsigned-int) 4)
    (:double 8)))

 ;; VAO declaration

(defun vao-find (name)
  (gethash name *vao-declarations*))

(defclass vertex-attribute ()
  ((name :initarg :name :reader vertex-attribute-name)
   (type :initarg :type)
   (out-type :initarg :out-type)
   (count :initarg :count)
   (normalizep :initarg :normalizep)))

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
    (%gl:vertex-attrib-divisor index divisor)))

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

(defclass vao-declaration ()
  ((attr-index :initform (make-hash-table))
   (attr-count :initform 0)
   (groups :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defun vao-decl-add-index (decl attr)
  (with-slots (attr-index attr-count) decl
    (setf (gethash (vertex-attribute-name attr) attr-index) attr-count)
    (incf attr-count)))

;;; VAO-ADD
(defmethod vao-add (vao object)
  (error "You may not add ~%  ~S~% to~%  ~S" object vao))

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

;;; ATTR/VBO counts
(defmethod vao-attr-count ((attr vertex-attribute)) 1)

(defmethod vao-attr-count ((group vertex-attribute-group))
  (with-slots (attributes) group
    (reduce #'+ (map 'list #'vao-attr-count attributes))))

(defmethod vao-attr-count ((vao vao-declaration))
  (with-slots (groups) vao
    (reduce #'+ (map 'list #'vao-attr-count groups))))

(defmethod vao-vbo-count ((attr vertex-attribute)) 1)
(defmethod vao-vbo-count ((group vertex-interleave-group)) 1)
(defmethod vao-vbo-count ((group vertex-block-group)) 1)
(defmethod vao-vbo-count ((group vertex-separate-group))
  (with-slots (attributes) group
    (reduce #'+ (map 'list #'vao-vbo-count attributes))))

(defmethod vao-vbo-count ((vao vao-declaration))
  (with-slots (groups) vao
    (reduce #'+ (map 'list #'vao-vbo-count groups))))

;;; ATTRIBUTE-SIZE

(defmethod attribute-size ((attr vertex-attribute))
  (with-slots (type count) attr
   (* (attribute-size type) count)))

(defmethod attribute-size ((group vertex-interleave-group))
  (with-slots (attributes divisor) group
    (loop for attr across attributes
          summing (attribute-size attr) into size
          finally (return (truncate size (if (= 0 divisor) 1 divisor))))))

(defmethod attribute-size ((group vertex-separate-group))
  (with-slots (attributes divisor) group
    (loop for attr across attributes
          summing (attribute-size attr) into size
          finally (return (truncate size (if (= 0 divisor) 1 divisor))))))

;;; VAO-SET-POINTERS
(defmethod vao-set-pointers ((group vertex-interleave-group) starting-index
                             vertex-count vbos)
  (declare (ignore vertex-count))
  (let ((stride (attribute-size group))
        (offset 0))
    (with-slots (attributes divisor) group
      (%gl:bind-buffer :array-buffer (aref vbos 0))
      (loop for attr across attributes
            for i from starting-index
            do (attribute-set-pointer attr i stride offset divisor)
               (incf offset (attribute-size attr))))))

(defmethod vao-set-pointers ((group vertex-separate-group) starting-index
                             vertex-count vbos)
  (declare (ignore vertex-count))
  (with-slots (attributes divisor) group
    (loop for attr across attributes
          for i from starting-index
          do (gl:bind-buffer :array-buffer (aref vbos i))
             (attribute-set-pointer attr i 0 0 divisor))))

(defmethod vao-set-pointers ((group vertex-block-group) starting-index
                             vertex-count vbos)
  (error "Implement VAO-SET-POINTERS for block groups"))

 ;; Parsing

(defvar *vao-decl* nil)

(defun vao-parse (list)
  ;; The distinction between a group decl and a vertex-attribute decl
  ;; is whether the second argument is an option list (which may be
  ;; NIL).
  (if (listp (cadr list))
      (vao-parse-group (vao-parse-make-group (car list) (cadr list))
                       (cddr list))
      (apply #'vao-parse-decl list)))

;;; VERTEX-ATTRIBUTE parsing

(defun vao-parse-decl (name type count &key out-type normalizep)
  (let ((attr (make-instance 'vertex-attribute
                :name name
                :type type
                :count count
                :out-type (or out-type type)
                :normalizep normalizep)))
    (vao-decl-add-index *vao-decl* attr)
    attr))

;;; VERTEX-ATTRIBUTE-GROUP parsing

(defgeneric vao-parse-make-group (type options))
(defgeneric vao-parse-group (group body))

(defmethod vao-parse-make-group ((type (eql :interleave)) options)
  (apply #'make-instance 'vertex-interleave-group options))

(defmethod vao-parse-make-group ((type (eql :blocks)) options)
  (apply #'make-instance 'vertex-block-group options))

(defmethod vao-parse-make-group ((type (eql :separate)) options)
  (apply #'make-instance 'vertex-separate-group options))

(defmethod vao-parse-group ((group vertex-attribute-group) body)
  (loop for i in body
        do (vao-add group (vao-parse i)))
  group)

 ;; DEFVAO

(defmacro defvao (name options &body groups)
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((*vao-decl* (make-instance 'vao-declaration)))
       (map 'nil (lambda (x)
                   (vao-add *vao-decl* (vao-parse x)))
            ',groups)
       (setf (gethash ',name *vao-declarations*) *vao-decl*))))

 ;; vao

(defclass ivao (vao)
  ((index :initarg :index)))

(defclass vao ()
  ((type :type vao-declaration)
   (id :initform (gl:gen-vertex-array))
   (vbos)
   (vertex-count :initform nil :initarg :vertex-count)
   (pointers :initform nil)
   (primitive :initarg :primitive :initform nil)))

(defmethod initialize-instance :after ((vao vao) &key type &allow-other-keys)
  (vao-bind vao)
  (with-slots ((vao-type type) id vbos vertex-count) vao
    (if type
        (setf vao-type (vao-find type))
        (error "No :TYPE specified for VAO."))
    (let ((vbo-count (vao-vbo-count vao-type)))
      (setf vbos (make-array vbo-count
                             :initial-contents (gl:gen-buffers vbo-count))))
    (with-slots (groups) vao-type
      (loop for group across groups
            as vbo-count = (vao-vbo-count group)
            as vbo-offset = 0 then (+ vbo-offset vbo-count)
            as vbo-subset = (make-array vbo-count :displaced-to vbos
                                                  :displaced-index-offset vbo-offset)
            as attr-offset = 0 then (+ attr-offset attr-count)
            as attr-count = (vao-attr-count group)
            do (loop for i from 0 below (vao-attr-count group)
                     do (%gl:enable-vertex-attrib-array i))
            (vao-set-pointers group attr-offset vertex-count vbo-subset)))))

(defmethod vao-attr-count ((vao vao))
  (with-slots (type) vao
    (vao-attr-count type)))

 ;; vao activation

(defun vao-bind (vao)
  (with-slots (id) vao
    (%gl:bind-vertex-array id)))

(defun vao-unbind ()
  (%gl:bind-vertex-array 0))

 ;; buffer-data

(defun vao-buffer-vector (vao vbo size vector &optional (usage :dynamic-draw))
  #+glkit-sv
  (with-slots (type vbos) vao
    (with-slots (attr-index) type
      (let* ((sv (static-vectors:make-static-vector
                  (length vector)
                  :element-type (array-element-type vector)
                  :initial-contents vector))
             (ptr (static-vectors:static-vector-pointer sv)))
        (%gl:bind-buffer :array-buffer (aref vbos vbo))
        (%gl:buffer-data :array-buffer size ptr usage)
        (static-vectors:free-static-vector sv))))
  #-glkit-sv
  (error "STATIC-VECTORS not supported by your implementation."))

(defun vao-buffer-data (vao vbo size pointer &optional (usage :dynamic-draw))
  (with-slots (type vbos) vao
    (with-slots (attr-index) type
      (%gl:bind-buffer :array-buffer (aref vbos vbo))
      (%gl:buffer-data :array-buffer size pointer usage))))

(defun vao-buffer-sub-vector (vao vbo offset size vector)
  #+glkit-sv
  (with-slots (type vbos) vao
    (with-slots (attr-index) type
      (let* ((sv (static-vectors:make-static-vector
                  (length vector)
                  :element-type (array-element-type vector)
                  :initial-contents vector))
             (ptr (static-vectors:static-vector-pointer sv)))
        (%gl:bind-buffer :array-buffer (aref vbos vbo))
        (%gl:buffer-sub-data :array-buffer offset size ptr)
        (static-vectors:free-static-vector sv))))
  #-glkit-sv
  (error "STATIC-VECTORS not supported by your implementation."))

(defun vao-buffer-sub-data (vao vbo offset size pointer)
  (with-slots (type vbos) vao
    (with-slots (attr-index) type
      (%gl:bind-buffer :array-buffer (aref vbos vbo))
      (%gl:buffer-sub-data :array-buffer offset size pointer))))

 ;; draw

(defun vao-draw (vao &key primitive (first 0) count)
  (with-slots ((prim primitive) vertex-count) vao
    (vao-bind vao)
    (%gl:draw-arrays (or primitive prim) first (or count vertex-count))))

(defun ivao-draw (vao &key primitive index)
  (with-slots ((prim primitive) (ind index)) vao
    (vao-bind vao)
    (gl:draw-elements (or primitive prim) (or index ind))))

 ;; delete

(defmethod gl-delete-object ((vao vao))
  (with-slots (vbos id) vao
    (gl:delete-buffers vbos)
    (gl:delete-vertex-arrays (list id))))
