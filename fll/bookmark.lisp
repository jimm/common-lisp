(in-package :fll)

(defclass bookmark ()
  ((url
    :initarg :url
    :initform ""
    :accessor url)
   (title
    :initarg :title
    :initform ""
    :accessor title)
   (comment
    :initarg :comment
    :initform ""
    :accessor comment)
   (approved
    :initarg :approved
    :initform t
    :accessor approved)))

(defclass bookmark-container ()
  ((bookmarks
    :initarg :bookmarks
    :initform '()
    :accessor bookmarks)))

;;; Generic method for creating favicon URL.
(defgeneric favicon-url  (url-container)
  (:documentation "Return favicon URL"))

;;; Given a URL, returns the favicon URL for its domain.
(defmethod favicon-url ((url string))
  (let* ((colon-loc (search "://" url))
	 (server-start (if colon-loc (+ colon-loc 3) 0))
	 (server-end (or
		      (search "/" url :start2 server-start)
		      (search "?" url :start2 server-start)
		      (length url))))
    (concatenate 'string
		 (cond (colon-loc "")
		       (t "http://"))
		 (subseq url 0 server-end)
		 "/favicon.ico")))

;;; Returns the favicon URL for a bookmark.
(defmethod favicon-url ((bookmark bookmark))
  (favicon-url (url bookmark)))

;;; Generic deny-bookmark
(defgeneric deny-bookmark (bookmark-container bookmark)
  (:documentation "Deny access to bookmark from bookmark-container by
removing it from bookmark-container."))

;;; Removes bookmark from person's list.
;;;
;;; To approve a bookmark, just mark it "approved". You don't have to do
;;; anything with the person.
(defmethod deny-bookmark ((container bookmark-container) (bookmark bookmark))
  (setf (bookmarks container)
	(remove bookmark (bookmarks container))))

;;; Generic unapproved-bookmarks
(defgeneric unapproved-bookmarks (bookmark-container)
  (:documentation "Return all unapproved bookmarks"))

;;; Returns the list of unapproved bookmarks a person has.
(defmethod unapproved-bookmarks ((container bookmark-container))
  (remove-if #'(lambda (b) (approved b))
	     (bookmarks container)))
