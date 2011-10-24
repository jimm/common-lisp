(in-package :fll)

(defclass server ()
  ((domain
    :initarg :domain
    :initform ""
    :reader domain)
   (families
    :initarg :families
    :initform '()
    :accessor families)))

;;; Returns the family that has UUID.
(defun family-for-uuid (server uuid)
  (find-if #'(lambda (member) (equal (uuid member) uuid))
	   (families server)))

;;; Returns the unique URL for a family.
(defun url-for-family (server family)
  (concatenate 'string
	       "http://" (domain server) "/" (uuid family)))
