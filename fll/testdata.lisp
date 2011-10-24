(in-package :fll)

(defmacro maybe (form)
  `(when (zerop (random 2)) ,form))

(defun make-test-url ()
  (concatenate 'string "http://www."
	       (loop repeat 12 collect (base36-to-char (random 26)))
	       ".com"))

(defun make-test-bookmarks (make-some-unapproved)
  (loop repeat 4 collect
	(make-instance 'bookmark
		       :url (make-test-url)
		       :title ""
		       :approved (cond
				  (make-some-unapproved
				   (zerop (random 2)))
				  (t t)))))

(defun make-test-person (role)
  (let ((person (make-instance 'person :username (gensym) :role role)))
    (setf (bookmarks person) (make-test-bookmarks t))
    person))

(defun make-test-child () (make-test-person :viewer))
(defun make-test-parent () (make-test-person :editor))

(defun make-test-family ()
  (let ((family (make-instance 'family))
	(members '()))
    (setf (name family) (gensym))
    (setf members (cons (make-test-parent) members))
    (maybe (setf members (cons (make-test-parent) members)))
    (loop repeat (random 5) do
	   (setf members (cons (make-test-child) members)))
    (setf (members family) members)
    (setf (bookmarks family) (make-test-bookmarks nil))
    family))

(defun make-test-server ()
  (let ((server (make-instance 'server)))
    (setf (families server) (loop repeat 20 collect (make-test-family)))
    server))
