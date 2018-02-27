#|
  This file is a part of cl-libsvm-format project.
|#

(defsystem "cl-libsvm-format"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "parse-float")
                 (:file "cl-libsvm-format" :depends-on ("parse-float")))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-libsvm-format-test"))))
