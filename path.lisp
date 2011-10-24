;;; Adapted from Practical Common Lisp by Peter Seibel.

(defun directory-pathname-p (p)
  "Tests whether a pathname is already in directory form."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))

(defun pathname-as-directory (name)
  "Converts any pathname to a directory form pathname."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun pathname-as-file (name)
  "The inverse of pathname-as-directory; converts pathname to a
file form pathname."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

(defun directory-wildcard (dirname)
  "Takes a pathname in either directory or file form and returns
a proper wildcard for the given implementation using read-time
conditionalization to make a pathname with a :wild type component
in all implementations except for CLISP and NIL in CLISP"
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Takes the name of a directory dirname and a function fn and
calls (fn file) on the pathnames of all the files under the
directory, recursively. It also takes two keyword
arguments: :directories and :test. When :directories is true, it
calls the function on the pathnames of directories as well as
regular files. The :test argument, if provided, specifies another
function that's invoked on each pathname before the main function
is; the main function is called only if the test function returns
true."
  (labels
      ((walk (name)
	     (cond
	      ((directory-pathname-p name)
	       (when (and directories (funcall test name))
		 (funcall fn name))
	       (dolist (x (list-directory name)) (walk x)))
	      ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
