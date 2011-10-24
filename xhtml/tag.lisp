(defstruct tag
  (name nil)
  (attrs '())
  (children '())
  )

;(defun find-tag-named (list name)
;  (cond ((null list) nil)
;	((equal (tag-name (car list)) name) (car list))
;	(t (find-tag-named (cdr list) name))))

;; Return the first child of tag with the given name.
(defun tag-first-child-named (tag name)
  (find-first #'tag-name (tag-children tag) name))

;; Add an attribute to the tag.
(defun tag-add-attr (tag attr)
  (tag-add-attrs tag (list attr)))

;; Add a list of attributes to the tag.
(defun tag-add-attrs (tag attrs)
  (setf (tag-attrs tag) (append (tag-attrs tag) attrs)))

;; Add a child tag to the tag.
(defun tag-add-child (tag child)
  (tag-add-children tag (list child)))

;; Add a list of children to the tag.
(defun tag-add-children (tag children)
  (setf (tag-children tag) (append (tag-children tag) children)))

;; Outputs the tag to a stream.
(defun tag-to-stream (tag stream)
  (if (null (tag-children tag)) (write-string (tag-empty tag) stream)
      (progn
	(write-string (tag-start tag) stream)
	(dolist (kid (tag-children tag))
	  (tag-to-stream kid stream))
	(write-string (tag-end tag) stream))))

;; Return the tag as a string.
(defun tag-as-string (tag) (as-string tag-to-stream tag))

;; Return the tag's start element as a string.
(defun tag-start (tag)
  (mkstr "<" (tag-name tag)
	 (when (tag-attrs tag) (mapcar 'attr-as-string (tag-attrs tag)) ">")))

;; Return the tag's end element as a string.
(defun tag-end (tag)
  (mkstr "</" (tag-name tag) ">"))

;; Return an
(defun tag-empty (tag)
  (mkstr "<" (tag-name tag) (mapcar 'attr-as-string (tag-attrs tag)) "/>"))
