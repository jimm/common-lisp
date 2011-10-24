(defstruct form
  (records '())
  (fields '())
  (user nil))

(defun make-form-for-user (user)
  (make-form :user user))
