(in-package :cl-user)
(defpackage pool
  (:use :cl)
  (:export :init
           :hello
           :drain
           :take
           :give
           :with-resource))
(in-package :pool)

(defun init (initial-size open-func close-func &optional health-check)
  (let ((pool (make-hash-table)))
    (setf
     (gethash :open-func pool) open-func
     (gethash :close-func pool) close-func
     (gethash :health-check pool) health-check
     (gethash :available pool)
     (loop for i from 1 to initial-size
        collect (funcall open-func)))
    pool))

(defun resource-health-check (pool resource)
  (let ((f (gethash :health-check pool)))
    (when f
      (assert (funcall f resource))))
  t)

(defun drain (pool)
  (mapc (gethash :close-func pool) (gethash :used pool))
  (mapc (gethash :close-func pool) (gethash :available pool))
  t)

(defun take (pool)
  (let ((rsrc (first (gethash :available pool))))
    (if rsrc
        (remove rsrc (gethash :available pool))
        (setf rsrc (funcall (gethash :open-func pool))))
    (unless (rsrc-health-check rsrc)
      (funcall (gethash :close-func pool) rsrc)
      (setf rsrc (funcall (gethash :open-func pool))))
    (push rsrc (gethash :used pool))
    rsrc))

(defun give (pool rsrc)
  (remove rsrc (gethash :used pool))
  (if (rsrc-health-check rsrc)
      (push rsrc (gethash :available pool)))
  t)

(defmacro with-resource (pool rsrc-sym &body body)
  `(let ((,rsrc-sym (take ,pool)))
     (unwind-protect
          (progn ,@body)
       (give ,pool ,rsrc-sym))))
