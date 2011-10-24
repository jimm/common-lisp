;;; NOTE: requires
;;; 1) path.lisp
;;; 2) cl-ppcre, a Perl-compatible regular expression library.
;;;    to install: (asdf-install:install 'cl-ppcre)
;;;    to use: (asdf:operate 'asdf:load-op 'cl-ppcre)

(defun with-every-line-in-file (f fn)
  "Performs (fn file line) on every line in file f. See walk-directory."
  (with-open-file (in f)
		  (loop for line = (read-line in nil)
			while line do (funcall fn f line))))

(defun with-every-line (dirname fn &key directories (test (constantly t)))
  "Performs (fn file line) on every line in dirname. See walk-directory."
  (walk-directory dirname
		  (lambda (f) (with-every-line-in-file f fn))
		  :test test))
		   
;; ****************************************************************

(defparameter *func-or-define-regex* "^\\s*(function\\s|define\\s*\\('?)(\\w+)")

(defun add-definition (dict definition)
  (setf (gethash definition dict) (cons definition (gethash definition dict))))

(defun definition-in-line (line)
  (multiple-value-bind (whole-match matches)
      (cl-ppcre:scan-to-strings *func-or-define-regex* line)
    (declare (ignore whole-match))
    (if (> (length matches) 1)
	(elt matches 1)
      nil)))

(defun find-funcs (dir)
  (let ((definitions (make-hash-table :test 'equal)))
    (with-every-line
     dir
     (lambda (file line)
       (declare (ignore file))
       (let ((definition (definition-in-line line)))
	 (when definition (add-definition definitions definition))))
     :test (lambda (f) (equal "php" (pathname-type f))))
    definitions))

(defun regex-match-p (str regex)
  (multiple-value-bind (whole-match matches)
      (cl-ppcre:scan-to-strings regex str)
    (declare (ignore matches))
    whole-match))

(defun remove-used-funcs (dir found-funcs)
  (let ((find-regexes (make-hash-table :test 'equal))
	(decl-regexes (make-hash-table :test 'equal)))
    (loop for key being the hash-keys of found-funcs
	  do (progn
	       (setf (gethash key find-regexes) (concatenate 'string "\\b" key "\\b"))
	       (setf (gethash key decl-regexes) (concatenate 'string "^\\s*(function\\s*|define\\s*\\('?)" key))))
    (with-every-line
	dir
	(lambda (file line)
	  (declare (ignore file))
	  (let ((found '()))
	    ; if key found on line but it's not a declaration, remember the key
	    (loop for key being the hash-keys of found-funcs
		  do (when (and (regex-match-p line (gethash key find-regexes))
				(not (regex-match-p line (gethash key decl-regexes))))
		       (setf found (cons key found))))
	    (loop for f in found
		  do (remhash f found-funcs))))
	:test (lambda (f) (equal "php" (pathname-type f)))))
  found-funcs)
		   
(defun unused-funcs (dir)
  (remove-used-funcs dir (find-funcs dir)))

;; ****************************************************************

(defparameter *otc*
  "/Users/jimm/src/oscmax/trunk/catalog")

;; testing

(defparameter *otcicf*
  "/Users/jimm/src/oscmax/trunk/catalog/iamplib/classes/functions")

(defparameter *otc-path*
  (make-pathname :directory '(:absolute "Users" "jimm" "src" "oscmax" "trunk"
					"catalog")))
(defparameter *otcicf-path*
  (make-pathname :directory '(:absolute "Users" "jimm" "src" "oscmax" "trunk"
					"catalog" "iamplib" "classes"
					"functions")))

(defun test-output-all-lines ()
  (walk-directory *otcicf*
		  (lambda (f) (format t "~a~%" f))
		  :test (lambda (f) (equal "php" (pathname-type f)))))
