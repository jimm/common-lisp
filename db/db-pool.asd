(in-package :cl-user)
(defpackage db-pool-asd
  (:use :cl :asdf))
(in-package :db-pool-asd)

(defsystem db-pool
  :version "0.1"
  :author "Jim Menard"
  :license "MIT"
  :depends-on (:pool :cl-dbi :cl-ppcre :quri)
  :components ((:module "src"
                :components ((:file "db-pool"))))
  :description "Generic resource pool for Common Lisp")
