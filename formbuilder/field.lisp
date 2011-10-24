(defstruct field
  (name ""))

(defun make-field-named (name)
  (make-field :name name))
