(in-package :fll)

;;; Translates a values from 0-35 into a character. Used when building uuids.
(defun base36-to-char (val)
  (code-char (cond ((< val 26) (+ val 97))	   ; a-z
		   ((< val 52) (+ (- val 26)  65)) ; A-Z
		   (t (+ (- val 52) 48)))))	   ; 0-9

;;; Creates a uuid.
(defun make-uuid ()
  (let ((vals (loop repeat 36 collect (random 62))))
    (concatenate 'string (mapcar 'base36-to-char vals))))

;;; Family class
(defclass family (bookmark-container)
  ((uuid
    :initarg :uuid
    :initform (make-uuid)
    :reader uuid)
   (name
    :initarg :name
    :initform ""
    :accessor name)
   (members
    :initarg :members
    :initform '()
    :accessor members)))

;;; Returns the list of family members that have one or more unapproved
;;; bookmarks.
(defun members-with-unapproved-bookmarks (family)
  (remove-if #'(lambda (m) (null (unapproved-bookmarks m)))
	     (members family)))
