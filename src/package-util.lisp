(defpackage #:kit.gl
  (:use #:cl #:alexandria))
(in-package #:kit.gl)

(defun package-external-symbols (package)
  (let (list)
    (do-external-symbols (s package list)
      (push s list))))

(defun ensure-use (pkg use-list)
  (let ((use-list (mapcar #'find-package use-list)))
    (unuse-package (set-difference (package-use-list pkg) use-list)
                   pkg)
    (use-package use-list pkg)))

(defun ensure-package (package-name &key nicknames use)
  (if-let (pkg (find-package package-name))
    (progn
      (ensure-use pkg use)
      (rename-package pkg (package-name pkg) nicknames))
    (make-package package-name :use use :nicknames nicknames)))

(defun ensure-export-only (export-list &optional (package *package*))
  (let ((package (find-package package)))
    (unexport (set-difference (package-external-symbols package)
                              export-list)
              package)
    (ensure-export export-list package)))

(defun ensure-export (export-list &optional (package *package*))
  (let ((package (find-package package)))
    (when export-list
      (export
       (mapcar (lambda (x) (ensure-symbol x package))
               export-list)
       package))))
