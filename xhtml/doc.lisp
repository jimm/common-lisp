;;; An XHTML document

(defstruct doc
  (head (make-tag :name "head"))
  (body (make-tag :name "body"))
  )

;; Create an XHTML document that contains a head with title and body.
(defun make-xhtml-document ()
  (let* ((doc (make-doc))
	 (head (doc-head doc))
	 title (make-tag :name "title"))
    (setf (tag-children head) (cons title (tag-children head)))
    doc
    ))

;; Return the document's title.
(defun doc-title (doc)
  (tag-first-child-named (doc-head doc) "title"))

;; Outputs the document to a stream.
(defun doc-to-stream (doc stream)
  (write-string (doc-xhtml-intro) stream)
  (tag-to-stream (doc-head doc) stream)
  (tag-to-stream (doc-body doc) stream))

;; Return the document as an xhtml string.
(defun doc-as-string (doc) (as-string doc-to-stream doc))

;; Return an XHTML document header (the XDecl and the html tag).
(defun doc-xhtml-intro ()
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html 
     PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
    \"DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">"
    )
