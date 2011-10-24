;;; Utilities

;; Make a string out of args, which may include lists and other objects.
(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args)
			       (if (listp a)
				   (dolist (inner a) (princ (mkstr inner) s))
				   (princ a s)))))

;; Returns the first list element where (func e.ement value) returns
;; non-nil.
(defun find-first (func list value)
  (cond ((endp list) nil)
	((equal (funcall func (car list)) value) (car list))
	(t (find-first func (cdr list) value))))

;; Given a character, returns a string which contains either the character
;; or the character escaped for XHTML.
(defun escaped (char)
  (cond ((equal #\& char) "&amp;")
	((equal #\< char) "&lt;")
	((equal #\> char) "&gt;")
	((equal #\" char) "&quot;")
	(t (string char))))

;; Returns a string with all occurrences of special XHTML characters
;; replaced with their XHTML values.
(defun escape-xhtml (str)
  (with-output-to-string (stream)
			 (loop for c across str do
			       (write-string (escaped c) stream))))

;;  Implements a <type>-as-string method that calls <type>-to-stream.
(defmacro as-string (to-stream-fun var-name)
  `(with-output-to-string (stream) (,to-stream-fun ,var-name stream)))


(defun tag-begin (tag attribs)
    (format nil "<~A ~A>" tag attribs))
(defun tag-end (tag)
    (format nil "</~A>" tag))

(defmacro defhtml-region (name &key
			       prepend-newline
			       append-newline)
  ;; This FORMAT is for debugging.
  ; (format t "~&defhtml-region ~A" name)
  `(defun ,name (&rest args)
     (let ((attribs (and (consp (first args))
			 (first args)))
	   (strings (if (consp (first args))
			(rest args)
		      args)))
       (apply #'concatenate 'string
	      (append ,(if prepend-newline
			   `'(,(format nil "~%"))
			 ())
		      (list (tag-begin ',name attribs))
		      strings
		      (list (tag-end ',name))
		      ,(if append-newline
			   `'(,(format nil "~%"))
			 ()))))))


(defhtml-region a)
(defhtml-region body :prepend-newline t)
(defhtml-region em)
(defhtml-region h1 :prepend-newline t)
(defhtml-region h2 :prepend-newline t)
(defhtml-region h3 :prepend-newline t)
(defhtml-region h4 :prepend-newline t)
(defhtml-region head :prepend-newline t)
(defhtml-region html :append-newline t)
(defhtml-region img)
(defhtml-region li :prepend-newline t)
(defhtml-region ol :prepend-newline t)
(defhtml-region p :prepend-newline t)
(defhtml-region strong)
(defhtml-region table :prepend-newline t)
(defhtml-region title :prepend-newline t)
(defhtml-region td)
(defhtml-region th)
(defhtml-region tr :prepend-newline t)
(defhtml-region ul)
