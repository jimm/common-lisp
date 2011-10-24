(defstruct attr
  (name nil)
  (value nil)
  )

(defun attr-set (attr name val)
  (setf (attr-name attr) name
	(attr-value attr) val))

(defun attr-to-stream (attr stream)
  (write-char #\Space stream)
  (write-string (attr-name attr) stream)
  (write-string "=\"" stream)
  (write-string (escape-xhtml (attr-value attr)) stream)
  (write-string "\"" stream))

(defun attr-as-string (attr) (as-string attr-to-stream attr))
