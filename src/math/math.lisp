(in-package :kit.glm)

(declaim (inline v))
(defun v (l)
  (etypecase l
    (sb-cga:vec
     l)
    (cons
     (sb-cga:vec (float (pop l) 1f0) (float (pop l) 1f0) (float (pop l) 1f0)))
    (vector
     (sb-cga:vec (float (aref l 0) 1f0) (float (aref l 1) 1f0)
                 (float (aref l 2) 1f0)))))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))


(defun frustum (left right bottom top near far)
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near))
        (2near (* 2 near)))
    (matrix (/ 2near r-l) 0.0 (/ (+ right left) r-l) 0.0
            0.0 (/ 2near t-b) (/ (+ top bottom) t-b) 0.0
            0.0 0.0 (- (/ (+ far near) f-n)) (/ (* -2 far near) f-n)
            0.0 0.0 -1.0 0.0)))

(defun perspective-matrix (fovy-degrees aspect z-near z-far)
  (let ((f (float (/ (tan (/ (deg-to-rad fovy-degrees) 2))) 1.0))
        (dz (- z-near z-far)))
    (matrix (/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
            0.0 0.0 -1.0 0.0)))

(defun ortho-matrix (left right bottom top near far)
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near)))
    (matrix (/ 2 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
            0.0 (/ 2 t-b) 0.0 (- (/ (+ top bottom) t-b))
            0.0 0.0 (/ -2 f-n) (- (/ (+ far near) f-n))
            0.0 0.0 0.0 1.0)))

(defun look-at (eye target up)
  (let* ((eye (v eye))
         (target (v target))
         (up (v up))
         (z (sb-cga:normalize (sb-cga:vec- target eye)))
         (x (sb-cga:normalize (sb-cga:cross-product z up)))
         (y (sb-cga:cross-product x z)))
    (sb-cga:matrix (aref x 0) (aref y 0) (- (aref z 0)) 0.0
                   (aref x 1) (aref y 1) (- (aref z 1)) 0.0
                   (aref x 2) (aref y 2) (- (aref z 2)) 0.0
                   0.0 0.0 0.0 1.0))
)
(declaim (inline copy-matrix))
(defun copy-matrix (m)
  (matrix (aref m 0) (aref m 4) (aref m 8) (aref m 12)
          (aref m 1) (aref m 5) (aref m 9) (aref m 13)
          (aref m 2) (aref m 6) (aref m 10) (aref m 14)
          (aref m 3) (aref m 7) (aref m 11) (aref m 15)))



