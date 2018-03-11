(in-package :cl-user)
(defpackage pool-asd
  (:use :cl :asdf))
(in-package :pool-asd)

(defsystem pool
  :version "0.1"
  :author "Jim Menard"
  :license "MIT"
  :components ((:module "src"
                :components ((:file "pool"))))
  :description "Generic resource pool for Common Lisp")
