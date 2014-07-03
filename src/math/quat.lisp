(in-package :kit.glm)

(declaim (inline vx vy vz))
(defun vx (v) (aref v 0))
(defun vy (v) (aref v 1))
(defun vz (v) (aref v 2))
(declaim (inline qw qi qj qk))
(defun qw (q) (aref q 0))
(defun qi (q) (aref q 1))
(defun qj (q) (aref q 2))
(defun qk (q) (aref q 3))

(deftype quaternion ()
  "A quaternion of single floats. [W, Xi, Yj, Zk]"
  '(simple-array single-float (4)))

(deftype dquaternion ()
  "A quaternion of double floats. [W, Xi, Yj, Zk]"
  '(simple-array double-float (4)))


(declaim (inline quaternion))
(defun quaternion (w x y z)
  "Allocate quaternion [W, Xi, Yj, Zk]."
  (make-array 4 :element-type 'single-float :initial-contents (list w x y z)))

(declaim (inline dquaternion))
(defun dquaternion (w x y z)
  "Allocate dquaternion [W, Xi, Yj, Zk]."
  (make-array 4 :element-type 'double-float :initial-contents (list w x y z)))


(declaim (inline angle-axis->quaternion))
(defun angle-axis->quaternion (angle-radians axis)
  "create a quaternion from specified axis and angle in radians"
  (let* ((half-a (/ angle-radians 2.0))
         (s (float (sin half-a) 1f0)))
    (quaternion (float (cos half-a) 1f0)
                (* (vx axis) s)
                (* (vy axis) s)
                (* (vz axis) s))))
;;; assuming this isn't generally speed sensitive, so not defaulting
;;; to inline
(declaim (notinline angle-axis->quaternion))

(declaim (inline angle-axis->dquaternion))
(defun angle-axis->dquaternion (angle-radians axis)
  "create a quaternion from specified axis and angle in radians"
  (let* ((half-a (/ angle-radians 2d0))
         (s (float (sin half-a) 1d0)))
    (dquaternion (float (cos half-a) 1d0)
                (* (vx axis) s)
                (* (vy axis) s)
                (* (vz axis) s))))
;;; assuming this isn't generally speed sensitive, so not defaulting
;;; to inline
(declaim (notinline angle-axis->d1quaternion))


#++
(defun euler->quaternion (rx ry rz)
  )

(defun quat->axis-angle (q)
  ;; from http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/index.htm
  (let* ((qx (qi q))
         (qy (qj q))
         (qz (qk q))
         (qw (qw q))
         (d (sqrt (- 1 (expt (qw q) 2))))
         (a (* 2 (acos qw))))
    (if (zerop a)
     (values a (sb-cga:vec 1.0 0.0 0.0))
     (values a (sb-cga:vec (/ qx d) (/ qy d) (/ qz d)))))
)
(declaim (inline copy-quaternion))
(defun copy-quaternion (q)
  (quaternion (qw q) (qi q) (qj q) (qk q)))
;;; assuming this isn't generally speed sensitive, so not defaulting
;;; to inline
(declaim (notinline copy-quat))


(declaim (inline %nq* nq* q*))
(defun %nq* (a1 a2 a3 a4 b1 b2 b3 b4)
  "multiply 2 quaternions with elements A{1..4} and B{1..4}, returning result as multiple values."
  (values  (- (* a1 b1)
              (* a2 b2)
              (* a3 b3)
              (* a4 b4))
           (+ (* a1 b2)
              (* a2 b1)
              (* a3 b4)
              (* a4 b3 -1))
           (+ (* a1 b3)
              (* a2 b4 -1)
              (* a3 b1)
              (* a4 b2))
           (+ (* a1 b4)
              (* a2 b3)
              (* a3 b2 -1)
              (* a4 b1))))

;; todo: convert this to magic compiler-macro transforms like sb-cga vec stuff
(defun %quat* (a b dest)
  "multiply quaternions A and B storing result into DEST."
  (setf
   (values (aref dest 0) (aref dest 1) (aref dest 2) (aref dest 3))
   (%nq* (aref a 0) (aref a 1) (aref a 2) (aref a 3)
         (aref b 0) (aref b 1) (aref b 2) (aref b 3))))

(defun quat* (a b &rest rest)
  "multiply quaternions A and B returning result as a new quaternion."
  (let ((r (multiple-value-call #'quaternion
             (%nq* (aref a 0) (aref a 1) (aref a 2) (aref a 3)
                   (aref b 0) (aref b 1) (aref b 2) (aref b 3)))))
    (when rest
      (loop for c in rest do (%quat* r c r)))
    r))

(defun dquat* (a b &rest rest)
  "multiply quaternions A and B returning result as a new quaternion."
  (let ((r (multiple-value-call #'dquaternion
             (%nq* (aref a 0) (aref a 1) (aref a 2) (aref a 3)
                   (aref b 0) (aref b 1) (aref b 2) (aref b 3)))))
    (when rest
      (loop for c in rest do (%quat* r c r)))
    r))


(defmacro defun-qrot (nname name index &optional post)
  (flet ((mul (i1 i2 &optional extra)
           (let ((term2 nil))
             (when post (rotatef i1 i2))
             (cond
               ((eq 'q1 i2)
                (setf term2 'cos-a/2))
               ((eq (elt '(q1 q2 q3 q4) index) i2)
                (setf term2 'sin-a/2)))
             (when term2
               `((* ,i1 ,term2 ,@(when extra (list extra))))))))
    (let ((body `((- ,@(mul 'q1 'q1)
                     ,@(mul 'q2 'q2)
                     ,@(mul 'q3 'q3)
                     ,@(mul 'q4 'q4))
                  (+ ,@(mul 'q1 'q2)
                     ,@(mul 'q2 'q1)
                     ,@(mul 'q3 'q4)
                     ,@(mul 'q4 'q3 -1))
                  (+ ,@(mul 'q1 'q3)
                     ,@(mul 'q2 'q4 -1)
                     ,@(mul 'q3 'q1)
                     ,@(mul 'q4 'q2))
                  (+ ,@(mul 'q1 'q4)
                     ,@(mul 'q2 'q3)
                     ,@(mul 'q3 'q2 -1)
                     ,@(mul 'q4 'q1))))
          (bindings '((a/2 (/ angle 2.0))
                      (cos-a/2 (cos a/2))
                      (sin-a/2 (sin a/2))
                      (q1 (aref quat 0))
                      (q2 (aref quat 1))
                      (q3 (aref quat 2))
                      (q4 (aref quat 3)))))
     `(progn
        (declaim (inline ,nname ,name))

        (defun ,nname (quat angle dest)
          (let* ,bindings
            (setf (values (aref dest 0)
                          (aref dest 1)
                          (aref dest 2)
                          (aref dest 3))
                  (values ,@body))
            dest))

        (defun ,name (quat angle)
          (let* ,bindings
            (quaternion ,@body)))))))


;;; macros to rotate a quat by specified angle:
;;;  local-* rotates around local axes of quat
;;;  world-* rotates around world space axes
;;;  rotate-* returns rotated quat without modifying args
;;;  nrotate* modifies (and returns) quat arg
;;; !note that these don't preserve -0.0 or NaNs properly when multiplying by 0

;; fixme: better names for these

;; inlined by macro
(defun-qrot q-nrotate-local-x q-rotate-local-x 1)
(defun-qrot q-nrotate-local-y q-rotate-local-y 2)
(defun-qrot q-nrotate-local-z q-rotate-local-z 3)

(defun-qrot q-nrotate-world-x q-rotate-world-x 1 t)
(defun-qrot q-nrotate-world-y q-rotate-world-y 2 t)
(defun-qrot q-nrotate-world-z q-rotate-world-z 3 t)


(defun quat-rotate-vector (quat vec)
  "rotate a vector VEC using specifed rotation quaternion Q, returning
result as a new single-float vector."
  (let ((q1 (aref quat 0))
        (q2 (aref quat 1))
        (q3 (aref quat 2))
        (q4 (aref quat 3)))
    (multiple-value-bind (vx v0 v1 v2)
        (multiple-value-bind (t1 t2 t3 t4)
            (%nq* q1 q2 q3 q4
                  0.0 (aref vec 0) (aref vec 1) (aref vec 2))
          (%nq* t1 t2 t3 t4
                q1 (- q2) (- q3) (- q4)))
      (declare (ignore vx))
      (sb-cga:vec v0 v1 v2)))
#++
  (let* ((c (make-array 3 :element-type 'single-float :initial-element 0.0)))
    (q-nrotate-3vector q v c)
    c))


;; todo: in place stuff
(declaim (inline quat+ quat-))
(defun quat+ (a b)
  (quaternion (+ (aref a 0) (aref b 0))
              (+ (aref a 1) (aref b 1))
              (+ (aref a 2) (aref b 2))
              (+ (aref a 3) (aref b 3))))

(defun quat- (a b)
  (quaternion (- (aref a 0) (aref b 0))
              (- (aref a 1) (aref b 1))
              (- (aref a 2) (aref b 2))
              (- (aref a 3) (aref b 3))))

(defun quat-inverse (quat)
  (quaternion (aref quat 0)
              (- (aref quat 1))
              (- (aref quat 2))
              (- (aref quat 3))))

(defun quat-rotate-matrix (quat &optional (matrix sb-cga:+identity-matrix+))
  (let* ((a (aref quat 0))
         (b (aref quat 1))
         (c (aref quat 2))
         (d (aref quat 3))
         (aa (* a a))
         (bb (* b b))
         (cc (* c c))
         (dd (* d d))
         (2ab (* 2 a b))
         (2ac (* 2 a c))
         (2ad (* 2 a d))
         (2bc (* 2 b c))
         (2bd (* 2 b d))
         (2cd (* 2 c d)))
    (etypecase quat
      (dquaternion
       (flet ((f (x) (float x 1.0)))
         (sb-cga:matrix*
          matrix
          (sb-cga:matrix
           (f (- (+ aa bb) cc dd)) (f(- 2bc 2ad)) (f(+ 2ac 2bd)) 0.0
           (f (+ 2ad 2bc))         (f (- (+ aa cc) bb dd)) (f (- 2cd 2ab)) 0.0
           (f (- 2bd 2ac))         (f (+ 2ab 2cd)) (f (- (+ aa dd) bb cc)) 0.0
           0.0 0.0 0.0 1.0))))
      (quaternion
       (sb-cga:matrix*
        matrix
        (sb-cga:matrix
         (- (+ aa bb) cc dd) (- 2bc 2ad)         (+ 2ac 2bd)         0.0
         (+ 2ad 2bc)         (- (+ aa cc) bb dd) (- 2cd 2ab)         0.0
         (- 2bd 2ac)         (+ 2ab 2cd)         (- (+ aa dd) bb cc) 0.0
         0.0 0.0 0.0 1.0))))))


;; todo: lerp, nlerp, slerp

(defun nqlerp (a b f)
  (let ((f2 (- 1.0 f)))
    ;; make sure we get shortest path between orientations
    ;; (if (a dot b) < 0, negate b)
    (let ((d (+ (* (aref a 0) (aref b 0))
                (* (aref a 1) (aref b 1))
                (* (aref a 2) (aref b 2))
                (* (aref a 3) (aref b 3)))))
      (when (< d 0)
        (map-into b #'- b)))
    (macrolet ((dim (n)
                 `(+ (* f2 (aref a ,n)) (* f (aref b ,n)))))
      (let* ((r0 (dim 0))
             (r1 (dim 1))
             (r2 (dim 2))
             (r3 (dim 3))
             (l (sqrt (+ (expt r0 2) (expt r1 2) (expt r2 2) (expt r3 2)))))
        (quaternion (float (/ r0 l) 1f0)
                    (float (/ r1 l) 1f0)
                    (float (/ r2 l) 1f0)
                    (float (/ r3 l) 1f0))))))
