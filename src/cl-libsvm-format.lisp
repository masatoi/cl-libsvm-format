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
  (max-dim 0 :type fixnum)
  label-type
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
   :label-type label-type))

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
                (:INDEX (tlet ((index (parse-integer cell) fixnum))
                          (when (> index (the fixnum (state-max-dim state)))
                            (setf (state-max-dim state) index))
                          index))
                (:LABEL (ecase (state-label-type state)
                          (fixnum (parse-integer cell))
                          (single-float (parse-float cell)))))))
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
  (values (state-result state)
          (state-max-dim state)))

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
