(in-package :cl-user)
(defpackage db-pool
  (:use :cl
        :cl-ppcre
        :pool)
  (:export :db-connection-params-from-url
           :init
           :disconnect
           :take
           :give
           :with-connection))
(in-package :db-pool)

(defun db-connection-params-from-url (url)
  (let* ((uri (quri:uri url))
         (username-and-password (cl-ppcre:split ":" (quri:uri-userinfo uri))))
    (list (intern (string-upcase (quri:uri-scheme uri)) "KEYWORD")
          :database-name (subseq (quri:uri-path uri) 1)
          :host (quri:uri-host uri)
          :username (first username-and-password)
          :password (second username-and-password)
          :port (quri:uri-port uri))))

(defun conn-health-check (conn)
  (let* ((query (dbi:prepare conn "select 1 as one"))
         (result (dbi:execute query))
         (row (dbi:fetch result)))
    (assert (= 1 (getf row :|one|))))
  t)

(defun init (initial-size db-params)
  (pool:init initial-size
             (lambda () (dbi:connect db-params))
             (lambda (conn) (dbi:disconnect conn))
             #'conn-health-check))

(defmacro with-connection (pool conn-sym &body body)
  `(let ((,conn-sym (pool:take ,pool)))
     (unwind-protect
          (progn ,@body)
       (pool:give ,pool ,conn-sym))))
