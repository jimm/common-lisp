(defstruct group
  selectable
  headers
  footers
  sort-order
  value
  (new-value-p t)
  (first-value-p t)
  record-count)

(defun group-initialize (g selectable)
  (let* ((header (make-section))
	 (f (make-field :type-string "selectable.fieldTypeString")))
    ;; Create new header section and add it to group's header list
    (setf (group-selectable g) selectable)
    (setf (group-headers g) (cons header (group-headers g)))

    ;; Create field for selectable. Make it bold.
    (setf (field-bold f) t)

    ;; Add field to header section.
    (setf (section-fields header) (cons f (section-fields header)))

    ;; Add empty footer section
    (setf (group-footers g) (cons (make-section) (group-footers g)))))

(defun group-reset (g)
  (setf (group-value g) nil)
  (setf (group-new-value-p g) t)
  (setf (group-first-value-p g) t)
  (setf (group-record-count g) 1))

