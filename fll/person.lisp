(in-package :fll)

(defclass person (bookmark-container)
  ((username
    :initarg :username
    :accessor username)
   (password
    :initarg :password
    :accessor password)
   (role
    :initarg :role
    :initform :viewer			; :viewer, :editor, or :admin
    :accessor role)))
