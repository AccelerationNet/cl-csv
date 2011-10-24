;; -*- lisp -*-

(cl:defpackage :cl-csv
  (:use :cl :cl-user :iterate)
  (:export :read-csv :csv-parse-error :format-csv-value
   :write-csv-value :write-csv-row :read-csv-row :write-csv :read-csv
   :*quote* :*separator* :*newline* :*quote-escape*

   ;; clsql stuff
   :export-query :import-from-csv :serial-import-from-csv
   :get-data-table-from-csv
   ))

(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

;;;; * Reading and Writing files in Comma-Seperated-Values format

;;;; Generating CSV files from lisp data

(defvar *quote* #\")
(defvar *separator* #\,)
(defvar *newline* #?"\r\n")
(defvar *always-quote* nil)
(defvar *quote-escape* #?"${ *quote* }${ *quote* }")

(defun white-space? (c)
  (member c '(#\newline #\tab #\space #\return)))

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

;;;; Writing csvs

(defun %out-stream (stream-or-string)
  (typecase stream-or-string
    (null (make-string-output-stream))
    (stream stream-or-string)
    (pathname
     (values
      (open stream-or-string :direction :output :if-exists :supersede)
      T))))

(defmethod format-csv-value (val)
  "Print values in ways that are most cross compatible with the csv format"
  (typecase val
    ((or float ratio) (format nil "~F" val))
    (string val)
    (null "")
    (T (princ-to-string val))))

(defmethod write-csv-value (val csv-stream
                            &key (formatter #'format-csv-value)
                            (quote *quote*)
                            (separator *separator*)
                            (escape *quote-escape*)
                            (always-quote *always-quote*)
                            &aux
                            (should-quote (or always-quote
                                              (iter (for char in-sequence (funcall formatter val))
                                                (thereis (or (char= quote char)
                                                             (char= separator char)))))))
  (when should-quote
    (write-char quote csv-stream))
  (iter
    (for char in-sequence (funcall formatter val))
    (if (char= quote char)
        (write-sequence escape csv-stream)
        (write-char char csv-stream)))
  (when should-quote
    (write-char quote csv-stream)))

(defmacro with-csv-output-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%out-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))

(defun write-csv-row (items
                      &key
                      stream
                      ((:separator *separator*) *separator*)
                      ((:quote *quote*) *quote*)
                      ((:escape *quote-escape*) *quote-escape*)
                      ((:newline *newline*) *newline*)
                      ((:always-quote *always-quote*) *always-quote*))
  "Write the list ITEMS to stream."
  (with-csv-output-stream (csv-stream stream)
    (iter (for item in items)
      (unless (first-iteration-p)
        (write-char *separator* csv-stream))
      (write-csv-value item csv-stream))
    (write-sequence *newline* csv-stream)
    (unless stream
      (get-output-stream-string csv-stream))))

(defun write-csv (rows-of-items
                  &key
                  stream
                  ((:separator *separator*) *separator*)
                  ((:quote *quote*) *quote*)
                  ((:escape *quote-escape*) *quote-escape*)
                  ((:newline *newline*) *newline*)
                  ((:always-quote *always-quote*) *always-quote*))
  (with-csv-output-stream (csv-stream stream)
    (iter (for row in rows-of-items)
      (write-csv-row row :stream csv-stream))
    (unless stream
      (get-output-stream-string csv-stream))))

;;;; Reading in CSV files

(defun %escape-seq? (s i escape llen elen)
  (declare (type simple-string escape)
           (type string s)
           (type fixnum i llen elen))
  (when (< (+ i elen) llen)
    (iter
      (declare (type fixnum eidx))
      (with eidx = 0)
      (always (char= (char escape eidx)
                     (char s (+ i eidx))))
      (incf eidx)
      (while (< eidx elen)))))

(defun %in-stream (stream-or-string)
  (typecase stream-or-string
    (string (make-string-input-stream stream-or-string))
    (stream stream-or-string)
    (pathname (values (open stream-or-string) T))))

(defmacro with-csv-input-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%in-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))

(defun read-csv-row
    (stream-or-string
     &key
     ((:separator *separator*) *separator*)
     ((:quote *quote*) *quote*)
     ((:escape *quote-escape*) *quote-escape*)
     &aux
     (current (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
     (state :waiting)
     line llen (c #\nul)
     (elen (length *quote-escape*)))
  "Read in a CSV by data-row (which due to quoted newlines may be more than one
                              line from the stream)
  "
  ;; giant state machine parser
  ;; states:
  ;;    waiting: we are between inputs, or have not started reading yet
  ;;    collecting: collecting unquoted data
  ;;    collecting-quoted: collecting quoted data
  ;;    waiting-on-next: done collecting quoted data, now waitin for a
  ;;        separator

  ;; this just ensures that a file opened here is closed here
  (with-csv-input-stream (in-stream stream-or-string)
    (iter
      (for i upfrom 0)
      (when (and line (< i llen))
        (setf c (elt line i)))
      (labels ((current-last-char ()
                 (elt current (- (fill-pointer current) 1)))
               (store-char (&optional char)
                 (vector-push-extend char current))
               (finish-item ()
                 ;; trim off unquoted whitespace at the end
                 (when (eql state :collecting)
                   (iter (while (white-space? (current-last-char)))
                     (decf (fill-pointer current))))
                 ;; collect the result
                 (collect (copy-seq (string current)) into items)
                 ;; go back to waiting for items
                 (setf state :waiting)
                 (setf (fill-pointer current) 0))
               (skip-escape ()
                 ;; we just read the first escape char
                 (incf i (- (length *quote-escape*) 1)))
               (read-line-in ()
                 (handler-case
                     ;; reset index, line and len for the next line of data
                     (setf i -1 ;; we will increment immediately after this
                           line (read-line in-stream)
                           llen (length line))
                   (end-of-file (sig)
                     (ecase state
                       (:waiting (error sig))
                       (:waiting-for-next (return items))
                       (:collecting (finish-item) (return items))
                       (:collecting-quoted (csv-parse-error sig)))
                     ))))
        (cond
          ;; if we dont have a line yet read one
          ((null line) (read-line-in))

          ;; we made it to the end of our line
          ((= i llen) ;; end of line
           (case state
             ;; in a quoted string that contains new-lines
             (:collecting-quoted
              (store-char #\newline)
              (read-line-in))
             (T (finish-item) (return items))))

          ;; the next characters are an escape sequence, start skipping
          ((and (eql state :collecting-quoted)
                *quote-escape* ;; if this is null there is no escape
                (%escape-seq? line i *quote-escape* llen elen))
           (store-char *quote*)
           (skip-escape))

          ;; the character is data separator, so gather the word unless
          ;; it is quoted data
          ((char= *separator* c)
           (ecase state
             (:collecting-quoted (store-char c))
             ((:collecting :waiting :waiting-for-next)
              (finish-item))))

          ;; the character is a quote (and not an escape) so start an item
          ;; finishing the item is the responsibility of separator/eol
          ((and *quote* (char= *quote* c))
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
              (unless (white-space? c)
                (setf state :collecting)
                (vector-push-extend c current)))
             (:waiting-for-next
              (unless (white-space? c)
                (csv-parse-error
                 "We finished reading a quoted value and got more characters before a separator or EOL ~D~%~A"
                 i line)))
             ((:collecting :collecting-quoted)
              (vector-push-extend c current))))
          )))))

(defun read-csv-sample (stream-or-string sample-size
                        &key row-fn map-fn skip-first-p
                        ((:separator *separator*) *separator*)
                        ((:quote *quote*) *quote*)
                        ((:escape *quote-escape*) *quote-escape*))
  (with-csv-input-stream (in-stream stream-or-string)
    (when skip-first-p (read-line in-stream))
    (iter
      (with sample = (make-array sample-size :initial-element nil))
      (for i from 0)
      (for data = (handler-case
                      (read-csv-row in-stream)
                    (end-of-file () nil)))
      (while data)
      (if (< i sample-size)
          (setf (aref sample i)
                (if map-fn (funcall map-fn data) data))
          (let ((r (random i)))
            (when (< r sample-size)
              (setf (aref sample r)
                    (if map-fn (funcall map-fn data) data)))))
      (finally
       (if row-fn
           (iter (for row in-vector sample) (funcall row-fn row))
           (return (coerce sample 'list)))))))

(defun read-csv (stream-or-string
                 &key row-fn map-fn sample skip-first-p
                 ((:separator *separator*) *separator*)
                 ((:quote *quote*) *quote*)
                 ((:escape *quote-escape*) *quote-escape*))
  "Read in a CSV by data-row (which due to quoted newlines may be more than one
                              line from the stream)

   row-fn: passing this parameter will cause this read to be streaming and results
           will be discarded after the row-fn is called with data

   map-fn: used for manipulating the data by row during collection if specified
           (funcall map-fn data) is collected instead of data
   sample: when a positive integer, only take that many samples from the input file
  "
  (with-csv-input-stream (in-stream stream-or-string)
    (when skip-first-p (read-line in-stream))

    (if sample
        (read-csv-sample in-stream sample :row-fn row-fn :map-fn map-fn)
        (iter
          (with data)
          (handler-case
              (setf data (read-csv-row in-stream))
            (end-of-file () (finish)))
          (if row-fn
              (funcall row-fn data)
              (collect (if map-fn (funcall map-fn data) data)))))))

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
