(in-package :kit.gl.tex)

(defmacro with-slot-values-override (slots instance &body body)
  (alexandria:once-only (instance)
    `(let (,@(loop for slot in slots
                   collect
                   (cond
                     ((symbolp slot)
                      `(,slot (or ,slot (slot-value ,instance ',slot))))
                     ((listp slot)
                      `(,(car slot) (or ,(car slot) (slot-value ,instance ',(cadr slot))))))))
       ,@body)))

(defmacro define-tex-fun (name slot-overrides other-keys &body body)
  `(defun ,name ,(concatenate 'list
                   (list 'texture '&key)
                   slot-overrides
                   other-keys)
     (with-slots (id) texture
       (with-slot-values-override ,slot-overrides texture
         ,@body))))
