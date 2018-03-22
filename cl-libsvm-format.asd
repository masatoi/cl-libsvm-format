#|
  This file is a part of cl-libsvm-format project.
|#

(defsystem "cl-libsvm-format"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "parse-float")
                 (:file "cl-libsvm-format" :depends-on ("parse-float")))))
  :description "A fast LibSVM data format reader for Common Lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op "cl-libsvm-format-test"))))
