(load "utils")

(html
 (head (title "title"))
 (body
  (h1 "title")
  (p "I'm a paragraph.")
  (p "I'm another.")
  (h2 '((align . center))
      "Attributes are keen.")))

;(load "attr")
;(load "tag")
;(load "doc")

; ================================================================
; (defun tag-print (tag) (format t "~S~%" (tag-as-string tag)))
;; (defun doc-print (doc) (format t "~S~%" (doc-as-string doc)))


;; (setq tag (make-tag :name "family"))

;; (tag-add-attrs tag (list (make-attr :name "husband" :value "bob")
;; 			 (make-attr :name "wife" :value "carol")))

;; (tag-add-children tag (list (make-tag :name "child-one")
;; 			    (make-tag :name "child-two")))

;; (setq doc (make-doc))
;; (tag-add-child (doc-body doc) tag)

;; (format t "~%")
;; ;(format t "~S~%"
;; ;	(with-output-to-string (stream) (doc-to-stream doc stream)))
;; (doc-to-stream doc *standard-output*)

; ================================================================
; example

;; (xhtml
;;  (head :title "Document Title")
;;  (body
;;   (h1 "Header One")
;;   (p "paragraph with " (bold "bold text") " and " (ital "italic text"))
;;   (p "another paragraph")
;;   (h2 "Header Two")
;;   (p "another paragraph")))
