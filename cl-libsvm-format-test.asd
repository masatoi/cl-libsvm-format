#|
  This file is a part of cl-libsvm-format project.
|#

(defsystem "cl-libsvm-format-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on ("cl-libsvm-format"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-libsvm-format"))))
  :description "Test system for cl-libsvm-format"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
