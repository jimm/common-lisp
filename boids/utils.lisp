;; Utilities

; Return a list that contains all of the elements of list except for the
; one eql to the first argument.
(defun all-but (element list)
  (remove-if #'(lambda (atom) (eql atom element)) list))
