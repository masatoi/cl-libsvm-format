(defpackage cl-libsvm-format-test
  (:use :cl
        :cl-libsvm-format
        :prove))
(in-package :cl-libsvm-format-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-libsvm-format)' in your Lisp.

(plan nil)

(is (car (parse-file (merge-pathnames
                      #P"tests/datasets/iris.scale"
                      (asdf:system-source-directory :cl-libsvm-format))))
    '(1 1 -0.555556 2 0.25 3 -0.864407 4 -0.916667)
    :test #'equalp)

(is (parse-file (merge-pathnames
                 #P"tests/datasets/float-label"
                 (asdf:system-source-directory :cl-libsvm-format)))
    '((96.1 1 -0.998916 2 -1.0 3 -0.822351 4 -0.973268 5 -0.979813 6 -0.960239 7
       -0.959704 8 -0.986756 9 -0.984752 10 -0.998866 11 -0.854494 12 -0.0520158)
      (95.2 1 -0.998916 2 -1.0 3 -0.955426 4 -0.977033 5 -0.978712 6 -0.980119 7
       -0.993284 8 -0.926592 9 -0.947685 10 -0.998157 11 -0.987471 12 0.52595997))
    :test #'equalp)

(finalize)
