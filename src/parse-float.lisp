;;; -*- coding:utf-8; mode:lisp -*-

(ql:quickload :alexandria)

(defpackage :svmformat.parse-float
  (:use :cl :alexandria)
  (:export #:parse-float))

(in-package :svmformat.parse-float)

(locally
    (declare (optimize (speed 3) (safety 0)))

  (deftype valid-radix ()
    "A valid Common Lisp radix."
    `(integer 2 36))

  (deftype bounding-index ()
    "A valid upper bound to a string."
    `(integer 0 ,array-total-size-limit))

  (deftype string-index ()
    "A valid string index."
    `(integer 0 ,(1- array-total-size-limit)))

  (declaim (type simple-base-string +whitespace-characters+))
  (alexandria:define-constant +whitespace-characters+
      (coerce '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page) 'simple-base-string)
    :test #'string=
    :documentation "List of whitespace characters")

  (declaim (inline sign-char-p))
  (defun sign-char-p (character)
    "Predicate for testing if CHARACTER is a sign character (i.e. #\+ or #\-)."
    (declare (type character character))
    (or (char= #\+ character)
        (char= #\- character)))

  (declaim (inline whitespace-char-p))
  (defun whitespace-char-p (character)
    "Predicate for testing if CHARACTER is a whitespace character."
    (declare (type character character))
    (loop :for c :across +whitespace-characters+
            :thereis (char= c character)))

  (defun parse-integer-only (string &key (start 0) (end (length string))
                                      (radix 10) (allow-sign t))
    "Parse an integer from a string, without skipping whitespaces.
Returns three values: the integer, the position in the string that
ended the parsing, and a boolean which is T if the parsing ended due
to a whitespace or end of the string, and NIL otherwise.  If
allow-sign is NIL (T by default), also signs are not allowed in the
string (i.e. cannot start with #\+ or #\-)."
    (declare (type (simple-array character) string)
             (type string-index start)
             (type bounding-index end)
             (type valid-radix radix))
    (let ((index start))
      (declare (type string-index index))
      (if (>= index end)
          (values nil index t)
          (let ((char (char string index)))
            (if (or (and (not allow-sign) (sign-char-p char))
                    (whitespace-char-p char))
                (values nil index t)
                (multiple-value-bind (value position)
                    (parse-integer string
                                   :start index
                                   :end end
                                   :junk-allowed t
                                   :radix radix)
                  (if (or (= position end)
                          (whitespace-char-p (char string position)))
                      (values value position t)
                      (values value position nil))))))))


  (defun parse-float (string &key (start 0) (end (length string))
                               (radix 10) (junk-allowed nil)
                               (decimal-character #\.))
    "Similar to PARSE-INTEGER, but parses a floating point value and
  returns the value as the specified TYPE (by default
  *READ-DEFAULT-FLOAT-FORMAT*). The DECIMAL-CHARACTER (by default #\.)
  specifies the separator between the integer and decimal parts, and
  the EXPONENT-CHARACTER (by default #\e, case insensitive) specifies
  the character before the exponent. Note that the exponent is only
  parsed if RADIX is 10."
    (declare (type (simple-array character) string)
             (type valid-radix radix)
             (type string-index start)
             (type bounding-index end)
             (type character decimal-character))
    (let* ((sign 1)                       ; sign of the float
           (digits 0)                     ; number of decimal digits
           (index start)
           (integer-part nil)             ; parts of the value
           (decimal-part 0)
           (result nil))                   ; final result
      (declare (type string-index index)
               (type integer sign)
               (type (or null (SIGNED-BYTE 64)) integer-part decimal-part))
      (labels ((parse-sign ()
                 (if (= index end)
                     #'parse-finish
                     (let ((char (char string index)))
                       (cond
                         ((char= #\- char)
                          (if (>= (incf index) end)
                              #'parse-finish
                              (progn
                                (setf sign -1)
                                #'parse-integer-part)))
                         ((char= #\+ char)
                          (if (>= (incf index) end)
                              #'parse-finish
                              #'parse-integer-part))
                         (t #'parse-integer-part)))))

               (parse-integer-part ()
                 (multiple-value-bind (value position finished)
                     (parse-integer-only string
                                         :start index
                                         :end end
                                         :radix radix
                                         :allow-sign nil)
                   (declare (type bounding-index position))
                   (setf integer-part value
                         index position)
                   (if finished
                       #'parse-finish
                       (let ((char (char string index)))
                         (cond
                           ((char= char decimal-character)
                            (incf index)
                            #'parse-decimal-part)
                           ((null integer-part)
                            #'parse-finish)
                           (t #'parse-finish))))))

               (parse-decimal-part ()
                 (multiple-value-bind (value position finished)
                     (parse-integer-only string
                                         :start index
                                         :end end
                                         :radix radix
                                         :allow-sign nil)
                   (declare (type bounding-index position))
                   (setf decimal-part (or value 0)
                         digits (- position index)
                         index position)
                   (when (and decimal-part
                              (null integer-part))
                     (setf integer-part 0))
                   (if finished
                       #'parse-finish
                       (progn
                         (unless decimal-part
                           (setf decimal-part 0))
                         #'parse-finish))))

               (parse-finish ()
                 (if integer-part
                     (if (or (= index end)
                             junk-allowed)
                         (setf result (* sign (+ (coerce integer-part 'single-float)
                                                 (* (coerce decimal-part 'single-float)
                                                    (expt (coerce radix 'single-float)
                                                          (coerce (- digits) 'single-float))))))
                         (simple-parse-error "junk in string ~S." string))
                     (unless junk-allowed
                       (simple-parse-error "junk in string ~S." string)))
                 nil))
        (declare (dynamic-extent #'parse-sign
                                 #'parse-integer-part
                                 #'parse-decimal-part
                                 #'parse-finish))

        (loop with parser = #'parse-sign
              while parser
              do (setf parser (funcall (the function parser)))
              finally (return (values result index)))))))

;; (time (loop repeat 1000000 do (parse-float:parse-float "123.3032")))

;; Evaluation took:
;;   0.315 seconds of real time
;;   0.316000 seconds of total run time (0.316000 user, 0.000000 system)
;;   100.32% CPU
;;   1,068,066,774 processor cycles
;;   0 bytes consed

;; (ql:quickload :parse-number)
;; (time (loop repeat 1000000 do (parse-number:parse-number "123.3032")))

;; Evaluation took:
;;   0.758 seconds of real time
;;   0.756000 seconds of total run time (0.756000 user, 0.000000 system)
;;   99.74% CPU
;;   2,570,571,153 processor cycles
;;   160,006,400 bytes consed

;; (time
;;  (loop repeat 1000000 do
;;    (with-input-from-string (in "123.3032")
;;      (read in))))

;; Evaluation took:
;;   0.686 seconds of real time
;;   0.688000 seconds of total run time (0.688000 user, 0.000000 system)
;;   100.29% CPU
;;   2,327,053,548 processor cycles
;;   160,000,800 bytes consed
