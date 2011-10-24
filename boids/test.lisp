(defun run-test-suite (test-func-list)
  (mapcar 'funcall test-func-list))

;; If the-test fails (evaluates to non-nil), print an error message.
(defmacro assert-nil (the-test &optional error-message)
  `(when ,the-test
     (if ,error-message (format t ,error-message)
       (format t "test ~S failed" ',the-test))))

;; If the-test fails (evaluates to nil), print an error message.
(defmacro assert-t (the-test &optional error-message)
  `(unless ,the-test
     (if ,error-message (format t ,error-message)
       (format t "test ~S failed" ',the-test))))

(defmacro assert-equal (answer the-test &optional error-message)
  `(assert-t (equal ,answer ,the-test) ,error-message))

(defmacro assert-eql (answer the-test &optional error-message)
  `(assert-t (eql ,answer ,the-test) ,error-message))

(defmacro assert-eq (answer the-test &optional error-message)
  `(assert-t (eq ,answer ,the-test) ,error-message))

(defmacro assert-= (answer the-test &optional error-message)
  `(assert-t (= ,answer ,the-test) ,error-message))
