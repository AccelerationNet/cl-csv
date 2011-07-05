;; -*- lisp -*-

(cl:defpackage :cl-csv
  (:use :cl :cl-user :iter)
  (:export :read-csv :csv-parse-error :format-csv-value
   :write-csv-value :write-line-csv :read-line-csv :write-csv :read-csv
   :*quote* :*separator* :*newline* :*quote-escape*))

(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

;;;; * Reading and Writing files in Comma-Seperated-Values format

;;;; Generating CSV files from lisp data

(defvar *quote* #\")
(defvar *separator* #\,)
(defvar *newline* #?"\r\n")
(defvar *quote-escape*
    #?"${ *quote* }${ *quote* }")

(defun dos-safe-read-line (stream &optional (eof-error-p t) eof-value recursive-p)
  "readline that can read unix or dos lines"
  (let ((line (read-line stream eof-error-p eof-value recursive-p)))
    (if (stringp line)
	(delete #\return line)
	line)))

(define-condition csv-parse-error (simple-condition)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report (lambda (c s)
	     (apply #'format
	      s
	      (format-control c)
	      (format-args c)))))

(defun csv-parse-error (msg &rest args)
  (error 'csv-parse-error :format-control msg :format-args args))

(defmethod format-csv-value (val)
  "Print values in ways that are most cross compatible with the csv format"
  (typecase val
    (float (format nil "~F" val))
    (string val)
    (T (princ-to-string val))))

(defmethod write-csv-value ( val csv-stream
                             &key (formatter #'format-csv-value)
                             (quote *quote*)
                             (escape *quote-escape*))
  (write-char quote csv-stream)
  (iter
    (for char in-sequence (funcall formatter val))
    (if (char= quote char)
        (write-sequence escape csv-stream)
        (write-char char csv-stream)))
  (write-char quote csv-stream))

(defun write-line-csv (items
                       &key
                       stream
                       (quote *quote*)
                       (escape *quote-escape*)
                       (separator *separator*)
                       (newline *newline*))
  "Write the list ITEMS to stream."
  (let ((csv-stream (or stream (make-string-output-stream))))
    (iter (for item in items)
      (unless (first-iteration-p)
        (write-char separator csv-stream))
      (write-csv-value item csv-stream :escape escape :quote quote))
    (write-sequence newline csv-stream)
    (unless stream
      (get-output-stream-string csv-stream))))

(defun write-csv (rows-of-items
                  &key
                  stream
                  (quote *quote*)
                  (escape *quote-escape*)
                  (separator *separator*)
                  (newline *newline*))
  (let ((csv-stream (or stream (make-string-output-stream))))
    (iter (for row in rows-of-items)
      (write-line-csv row :stream csv-stream :quote quote :separator separator
                          :escape escape :newline newline))
    (unless stream
      (get-output-stream-string csv-stream))))

;;;; Reading in CSV files

(defun %char-at (s i)
  (when (< i (length s)) (elt s i)))

(defun %escape-seq? (s i escape)
  (iter (for c in-string escape)
    (for offset upfrom 0)
    (always (eql c (%char-at s (+ i offset))))))

(defun read-line-csv (line &key
                           (separator *separator*)
                           (quote *quote*)
                           (escape *quote-escape*))
  "Parse a csv line into a list of strings using seperator as the
  column seperator and quote as the string quoting character."
  (iter
    (with state = :waiting)
    (with current = (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
    (with skip = 0)
    (with llen = (length line))
    (for i upfrom 0)
    (for c = (%char-at line i))
    (for prev-c previous c)

    (flet ((finish-item ()
             (collect (copy-seq (string current)) into items)
             (setf state :waiting)
             (setf (fill-pointer current) 0))
           (skip-escape ()
             ;; we just read the first escape char
             (setf skip (- (length escape) 1))
             (setf state :escaping)))
      (cond
        ((= i llen) ;; end of line, finish this out and return the items
         (case state
           (:collecting-quoted
            (csv-parse-error "Unexpected EOL while reading a quoted item from csv~%~A"
                             line))
           (T (finish-item)))
         (return items))

        ;; this state handles skipping the characters in an escape sequence
        ((eql state :escaping)
         (when (zerop (decf skip)) (setf state :collecting-quoted))
         (next-iteration))

        ;; the next characters are an escape sequence, start skipping
        ((%escape-seq? line i escape)
         (case state
           (:collecting-quoted
            (vector-push-extend quote current)
            (skip-escape))
           (T (csv-parse-error "Found an escape sequence where it was not expected ~A:~D~%~A"
                               state i line ))))

        ;; the character is data separator, so gather the word unless
        ;; it is quoted data
        ((char= separator c)
         (ecase state
           (:collecting-quoted
            (vector-push-extend c current))
           ((:collecting :waiting :waiting-for-next)
            (finish-item))))

        ;; the character is a quote so start an item
        ;; finishing the item is the responsibility of separator/eol
        ((char= quote c)
         (ecase state
           (:waiting (setf state :collecting-quoted))
           (:collecting-quoted
            ;; if we end up trying to read
            (setf state :waiting-for-next))
           (:collecting
             (csv-parse-error "we are reading non quoted csv data and found a quote at ~D~%~A"
                              i line))))
        (T
         (ecase state
           (:waiting
            (setf state :collecting)
            (vector-push-extend c current))
           (:waiting-for-next
            (csv-parse-error
             "We finished reading a quoted value and got more characters before a separator or EOL ~D~%~A"
             i line))
           ((:collecting :collecting-quoted)
            (vector-push-extend c current))))
        ))))

(defun read-csv (stream-or-string
                 &key
                 row-fn
                 (separator *separator*)
                 (quote *quote*)
                 (escape *quote-escape*))
  (let ((stream (typecase stream-or-string
                  (string (make-string-input-stream stream-or-string))
                  (stream stream-or-string))))
    (iter (for line in-stream stream using #'dos-safe-read-line)
      (for data = (read-line-csv line :separator separator :escape escape :quote quote))
      (if row-fn
          (funcall row-fn data)
          (collect data)))))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
