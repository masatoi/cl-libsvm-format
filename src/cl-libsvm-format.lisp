;;; -*- coding:utf-8; mode:lisp -*-

(defpackage :cl-libsvm-format
  (:use :cl :svmformat.parse-float)
  (:nicknames :svmformat)
  (:export #:parse-file #:parse-stream))

(in-package :svmformat)

(declaim (inline extend-field cell-input cell-finish row-finish parse finish))

(eval-when (:compile-toplevel)
  (defparameter *optimize-settings*
    '(optimize (speed 3) (safety 0) (debug 0))))

(defmacro defn (function-spec (&rest arg-specs) &body body)
  (assert (listp function-spec))
  (assert (listp arg-specs))
  (dolist (arg-spec arg-specs)
    (assert (listp arg-spec))
    (assert (= (length arg-spec) 2)))
  `(progn
     (declaim (ftype (function ,(mapcar #'cadr arg-specs) ,(cadr function-spec)) ,(car function-spec)))
     (defun ,(car function-spec) ,(mapcar #'car arg-specs)
       (declare ,*optimize-settings*
                ,@(mapcar (lambda (arg arg-type)
                            (list 'type arg-type arg))
                          (mapcar #'car arg-specs)
                          (mapcar #'cadr arg-specs)))
       ,@body)))

(defmacro tlet (bindings &body body)
  (assert (listp bindings))
  (dolist (binding bindings)
    (assert (listp binding))
    (assert (= (length binding) 3)))
  `(let (,@(mapcar (lambda (binding)
                     (subseq binding 0 2))
                   bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))

(defstruct (state (:constructor %make-state))
  (buffer "" :type (simple-array character))
  (field  "" :type (simple-array character))
  (offset      0 :type fixnum)
  (space-count 0 :type fixnum)
  (tag :START :type keyword)
  label-parser
  row
  row-ptr
  result
  result-ptr)

(defun print-state1 (state)
  (format t "tag: ~A, offset: ~A~%"
          (state-tag state) (state-offset state)))

(defun print-state2 (state)
  (format t "field: ~A~%"
          (state-field state)))

(defun print-state3 (state)
  (format t "row: ~A, row-ptr: ~A, result: ~A, result-ptr: ~A~%"
          (state-row state) (state-row-ptr state)
          (state-result state) (state-result-ptr state)))

(defun make-state (buffer-size field-size &key (label-type 'fixnum))
  (%make-state
   :buffer (make-string buffer-size :initial-element #\Null)
   :field  (make-string field-size  :initial-element #\Null)
   :label-parser (ecase label-type
                   (fixnum #'parse-integer)
                   (single-float #'parse-float))))

(defn (cell-input null) ((state state) (char character))
  (tlet ((field  (state-field state)  (simple-array character))
         (offset (state-offset state) fixnum))
    (when (>= offset (length field)) (error "too large field size"))
    (setf (char field offset) char)
    (incf (state-offset state))
    nil))

(defn (cell-finish null) ((state state))
  (tlet ((offset (state-offset state) fixnum)
         (field (state-field state) (simple-array character)))
    (tlet ((cell (subseq field 0 offset) (simple-array character)))
      (let ((cell-data
              (case (state-tag state)
                (:VALUE (parse-float cell))
                (:INDEX (parse-integer cell))
                (:LABEL (funcall (state-label-parser state) cell)))))
        (setf (state-offset state) 0) ; initialize offset
        (if (null (state-row state))
            (setf (state-row state)           (cons cell-data nil)
                  (state-row-ptr state)       (state-row state))
            (setf (cdr (state-row-ptr state)) (cons cell-data nil)
                  (state-row-ptr state)       (cdr (state-row-ptr state))))
        nil))))

(defn (row-finish null) ((state state))
  (tlet ((offset (state-offset state) fixnum))
    (when (> offset 0)
      (cell-finish state))
    (if (null (state-result state))
        (setf (state-result state)           (cons (state-row state) nil)
              (state-result-ptr state)       (state-result state))
        (setf (cdr (state-result-ptr state)) (cons (state-row state) nil)
              (state-result-ptr state)       (cdr (state-result-ptr state))))
    (setf (state-row state) nil)
    nil))

(defn (parse state) ((size fixnum) (state state))
  (tlet ((buffer (state-buffer state) (simple-array character)))
    (loop for i fixnum from 0 below size
          for c character across buffer do
            (tagbody
               ;; (print-state1 state)
               ;; (print-state2 state)
               ;; (print-state3 state)
               (when (eq (state-tag state) :START)
                 (cond ((or (char= c #\Space) (char= c #\Tab)) ; skip blank
                        (go :CONTINUE))
                       (t
                        (setf (state-tag state) :LABEL))))
               ;; processing field
               (case (state-tag state)
                 (:LABEL
                  (cond ((char= c #\Space)
                         (cell-finish state)
                         (setf (state-tag state) :INTERVAL)
                         (go :CONTINUE))
                        (t
                         (cell-input state c))))
                 (:INTERVAL
                  (cond ((or (char= c #\Space) (char= c #\Tab)) ; skip blank
                         (go :CONTINUE))
                        ((or (char= c #\Newline) (char= c #\Linefeed))
                         (row-finish state)
                         (setf (state-tag state) :START)
                         (go :CONTINUE))
                        (t (cell-input state c)
                           (setf (state-tag state) :INDEX)
                           (go :CONTINUE))))
                 (:INDEX
                  (cond ((char= c #\:)
                         (cell-finish state)
                         (setf (state-tag state) :VALUE)
                         (go :CONTINUE))
                        (t (cell-input state c)
                           (go :CONTINUE))))
                 (:VALUE
                  (cond ((char= c #\Space)
                         (cell-finish state)
                         (setf (state-tag state) :INTERVAL)
                         (go :CONTINUE))
                        ((or (char= c #\Newline) (char= c #\Linefeed))
                         (print c)
                         (row-finish state)
                         (setf (state-tag state) :START)
                         (go :CONTINUE))
                        (t (cell-input state c)
                           (go :CONTINUE)))))
             :CONTINUE))
    state))

(defun finish (state)
  (when (> (state-offset state) 0)
    (row-finish state))
  (state-result state))

(defun parse-stream (stream &key (buffer-size 8192) (field-size 1024) (label-type 'fixnum))
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (declare (type fixnum buffer-size field-size))
  (let ((state (make-state buffer-size field-size :label-type label-type)))
    (loop for size = (read-sequence (state-buffer state) stream)
          until (= size 0)
          do (parse size state)
          finally (return (finish state)))))

(defun first-label-integer-p (file &key (external-format #+clisp system::*default-file-encoding*
                                                     #+allegro excl:*default-external-format*
                                                     #+sbcl :utf-8))
  (with-open-file (stream file :direction :input :external-format external-format
                               #+clisp :buffered #+clisp t
                               #+allegro :mapped #+allegro t)
    (let* ((line (read-line stream))
           (label (subseq line 0 (position #\Space line))))
      (handler-case
          (parse-integer label)
        (error (e)
          (declare (ignore e))
          nil)
        (t (result)
          result)))))

(defun parse-file (file &key (external-format #+clisp system::*default-file-encoding*
                                              #+allegro excl:*default-external-format*
                                              #+sbcl :utf-8)
                          (buffer-size 8192) (field-size 1024))
  (let ((label-type
          (if (first-label-integer-p file :external-format external-format)
              'fixnum
              'single-float)))
    (if (null (probe-file file))
        (error "can't open file")
        (with-open-file (stream file :direction :input :external-format external-format
                                     #+clisp :buffered #+clisp t
                                     #+allegro :mapped #+allegro t)
          (parse-stream stream :buffer-size buffer-size :field-size field-size :label-type label-type)))))

;; (require :sb-sprof)

;; (sb-sprof:with-profiling (:max-samples 1000
;;                           :report :flat
;;                           :loop nil)
;;   (time (progn (svmformat:parse-file "/home/wiz/datasets/mnist.scale")
;;                nil)))

;; Profiler sample vector full (337 traces / 10000 samples), doubling the size
;; Evaluation took:
;;   4.257 seconds of real time
;;   4.260000 seconds of total run time (4.244000 user, 0.016000 system)
;;   [ Run times consist of 0.220 seconds GC time, and 4.040 seconds non-GC time. ]
;;   100.07% CPU
;;   14,439,062,341 processor cycles
;;   1,136,466,784 bytes consed
  

;; Number of samples:   359
;; Sample interval:     0.01 seconds
;; Total sampling time: 3.59 seconds
;; Number of cycles:    0
;; Sampled threads:
;;  #<SB-THREAD:THREAD "worker" RUNNING {1005CD63B3}>

;;            Self        Total        Cumul
;;   Nr  Count     %  Count     %  Count     %    Calls  Function
;; ------------------------------------------------------------------------
;;    1     86  24.0    121  33.7     86  24.0        -  PARSE-INTEGER
;;    2     76  21.2    359 100.0    162  45.1        -  PARSE-STREAM
;;    3     49  13.6    151  42.1    211  58.8        -  SVMFORMAT.PARSE-FLOAT::PARSE-INTEGER-ONLY
;;    4     41  11.4     41  11.4    252  70.2        -  SB-IMPL::FD-STREAM-READ-N-CHARACTERS/UTF-8
;;    5     30   8.4     30   8.4    282  78.6        -  DIGIT-CHAR-P
;;    6     17   4.7    202  56.3    299  83.3        -  PARSE-FLOAT
;;    7     15   4.2     15   4.2    314  87.5        -  SB-VM::ALLOC-TRAMP
;;    8     12   3.3     90  25.1    326  90.8        -  (LABELS SVMFORMAT.PARSE-FLOAT::PARSE-DECIMAL-PART :IN PARSE-FLOAT)
;;    9      8   2.2      9   2.5    334  93.0        -  (LABELS SVMFORMAT.PARSE-FLOAT::PARSE-FINISH :IN PARSE-FLOAT)
;;   10      6   1.7     79  22.0    340  94.7        -  (LABELS SVMFORMAT.PARSE-FLOAT::PARSE-INTEGER-PART :IN PARSE-FLOAT)
;;   11      6   1.7      6   1.7    346  96.4        -  SB-IMPL::GET-CAT-ENTRY
;;   12      5   1.4      5   1.4    351  97.8        -  SB-KERNEL:UB32-BASH-COPY
;;   13      4   1.1      4   1.1    355  98.9        -  "foreign function pow"
;;   14      2   0.6     48  13.4    357  99.4        -  SB-IMPL::ANSI-STREAM-READ-STRING-FROM-FRC-BUFFER
;;   15      0   0.0    359 100.0    357  99.4        -  PARSE-FILE
;;   16      0   0.0    359 100.0    357  99.4        -  (LAMBDA NIL :IN #:DROP-THRU-TAG-1)
;;   17      0   0.0    359 100.0    357  99.4        -  SB-EXT:CALL-WITH-TIMING
;;   18      0   0.0    359 100.0    357  99.4        -  "Unknown component: #x1005A71050"
;;   19      0   0.0    359 100.0    357  99.4        -  SB-INT:SIMPLE-EVAL-IN-LEXENV
;;   20      0   0.0    359 100.0    357  99.4        -  EVAL
;;   21      0   0.0    359 100.0    357  99.4        -  (LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL)
;;   22      0   0.0    359 100.0    357  99.4        -  SWANK::CALL-WITH-RETRY-RESTART
;;   23      0   0.0    359 100.0    357  99.4        -  SWANK::CALL-WITH-BUFFER-SYNTAX
;;   24      0   0.0    359 100.0    357  99.4        -  SWANK:EVAL-FOR-EMACS
;;   25      0   0.0    359 100.0    357  99.4        -  (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD)
;;   26      0   0.0    359 100.0    357  99.4        -  SWANK/SBCL::CALL-WITH-BREAK-HOOK
;;   27      0   0.0    359 100.0    357  99.4        -  (FLET SWANK/BACKEND:CALL-WITH-DEBUGGER-HOOK :IN "/home/wiz/.roswell/lisp/quicklisp/dists/quicklisp/software/slime-v2.20/swank/sbcl.lisp")
;;   28      0   0.0    359 100.0    357  99.4        -  SWANK::CALL-WITH-BINDINGS
;;   29      0   0.0    359 100.0    357  99.4        -  (FLET SB-UNIX::BODY :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE)
;;   30      0   0.0    359 100.0    357  99.4        -  (FLET "WITHOUT-INTERRUPTS-BODY-4" :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE)
;;   31      0   0.0    359 100.0    357  99.4        -  (FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE)
;;   32      0   0.0    359 100.0    357  99.4        -  (FLET "WITHOUT-INTERRUPTS-BODY-1" :IN SB-THREAD::CALL-WITH-MUTEX)
;;   33      0   0.0    359 100.0    357  99.4        -  SB-THREAD::CALL-WITH-MUTEX
;;   34      0   0.0    359 100.0    357  99.4        -  SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE
;;   35      0   0.0    359 100.0    357  99.4        -  "foreign function call_into_lisp"
;;   36      0   0.0    359 100.0    357  99.4        -  "foreign function new_thread_trampoline"
;;   37      0   0.0     48  13.4    357  99.4        -  SB-IMPL::ANSI-STREAM-READ-SEQUENCE
;;   38      0   0.0     48  13.4    357  99.4        -  READ-SEQUENCE
;;   39      0   0.0     41  11.4    357  99.4        -  SB-INT:FAST-READ-CHAR-REFILL
;; ------------------------------------------------------------------------
;;           2   0.6                                     elsewhere
