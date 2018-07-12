;; -*- lisp -*-
(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

;;;; * Reading and Writing files in Comma-Seperated-Values format

;;;; Generating CSV files from lisp data

(defun white-space? (c)
  (member c '(#\newline #\tab #\space #\return)))

(define-condition csv-parse-error (error)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report (lambda (c s &aux (ctrl (format-control c)))
	     (typecase ctrl
               (condition
                (format s "CSV-PARSE-ERROR: internal-error ~A" ctrl))
               (string
                (apply #'format s ctrl (format-args c)))))))

(defun csv-parse-error (msg &rest args)
  (error 'csv-parse-error :format-control msg :format-args args))

(define-condition csv-data-read ()
  ((data :accessor data :initarg :data :initform nil)
   (csv-reader :accessor csv-reader :initarg :csv-reader :initform nil )))

(defun csv-data-read ( data &key csv-reader )
  (if *enable-signals*
      (restart-case
          (progn (signal 'csv-data-read :data data :csv-reader csv-reader ) data)
        (filter (new-data) new-data))
      data))

(define-condition csv-row-read ()
  ((row :accessor row :initarg :row :initform nil)
   (csv-reader :accessor csv-reader :initarg :csv-reader :initform nil )))

(defun csv-row-read ( row &key csv-reader)
  (if *enable-signals*
      (restart-case
          (progn (signal 'csv-row-read :row row :csv-reader csv-reader ) row)
        (filter (new-row) new-row))
      row))

;;;; Writing csvs

(defgeneric format-csv-value (val)
  (:documentation "Print values in ways that are most cross compatible with the csv format")
  (:method (val)
    (typecase val
      ((or float ratio) (format nil "~F" val))
      (string val)
      (null "")
      (t (princ-to-string val)))))

(defun %char-in (c to-check)
  (typecase to-check
    (character (char= c to-check))
    (string
     (iter (for c2 in-string to-check)
       (thereis (char= c c2))))))

(defun chars-in (chars-to-check value-to-look-through)
  "returns true if any of the chars-to-check is found in the value-to-look-through"
  (iter (for c1 in-string value-to-look-through)
    (thereis
     (iter (for to-check in (alexandria:ensure-list chars-to-check))
       (thereis (%char-in c1 to-check))))))

(defgeneric write-csv-value (val csv-stream
                             &key formatter quote separator escape always-quote)
  (:documentation "Writes val to csv-stream in a formatted fashion.

Keywords

formatter: used to format val. Defaults to format-csv-value.

quote: quoting character. Defaults to *quote*

escape: escaping character. Defaults to *quote-escape*

newline: newline character. Defaults to *write-newline*

always-quote: Defaults to *always-quote*")
  (:method (val csv-stream
            &key (formatter #'format-csv-value)
            (quote *quote*)
            (separator *separator*)
            (escape *quote-escape*)
            (always-quote *always-quote*)
            (newline *write-newline*)
            &aux
            (escape (or escape #(#\" #\")))
            (formatted-value (funcall formatter val))
            (should-quote (or always-quote
                              (chars-in (list quote separator newline)
                                        formatted-value))))
    (when should-quote
      (write-char quote csv-stream))
    (iter
      (for char in-sequence formatted-value)
      (if (char= quote char)
          (write-sequence escape csv-stream)
          (write-char char csv-stream)))
    (when should-quote
      (write-char quote csv-stream))))

(defmacro with-csv-output-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%out-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))

(defun %out-stream (stream-or-string)
  "creates a stream from the given thing, trying to DWIM"
  (etypecase stream-or-string
    (null (make-string-output-stream))
    (stream stream-or-string)
    (pathname
     (values
      (open stream-or-string :direction :output :if-exists :supersede)
      t))))

(defun write-csv-row (items
                      &key
                      stream
                      ((:separator *separator*) *separator*)
                      ((:quote *quote*) *quote*)
                      ((:escape *quote-escape*) *quote-escape*)
                      ((:newline *write-newline*) *write-newline*)
                      ((:always-quote *always-quote*) *always-quote*))
"
Writes a list items to stream

rows-of-items: iterable

Keywords:

stream: stream to write to. Default: nil.

quote: quoting character. Defaults to *quote*

escape: escaping character. Defaults to *quote-escape*

newline: newline character. Defaults to *write-newline*

always-quote: Defaults to *always-quote*"
  (with-csv-output-stream (csv-stream stream)
    (iter (for item in (alexandria:ensure-list items))
      (unless (first-iteration-p)
        (write-char *separator* csv-stream))
      (write-csv-value item csv-stream))
    (write-sequence *write-newline* csv-stream)
    (unless stream
      (get-output-stream-string csv-stream))))

(defun write-csv (rows-of-items
                  &key
                  stream
                  ((:separator *separator*) *separator*)
                  ((:quote *quote*) *quote*)
                  ((:escape *quote-escape*) *quote-escape*)
                  ((:newline *write-newline*) *write-newline*)
                  ((:always-quote *always-quote*) *always-quote*))
  "Writes a csv to the given stream.

  rows-of-items: iterable

  Keywords:
    stream: stream to write to. Default: nil.
      nil - writes the rows to a string and returns it
      an open stream
      a pathname (overwrites if the file exists)
    quote: quoting character. Defaults to *quote*
    escape: escaping character. Defaults to *quote-escape*
    newline: newline character. Defaults to *write-newline*
    always-quote: Defaults to *always-quote*"
  (with-csv-output-stream (csv-stream stream)
    (iter (for row in rows-of-items)
      (write-csv-row row :stream csv-stream))
    (unless stream
      (get-output-stream-string csv-stream))))

;;;; Reading in CSV files

(defun %escape-seq? (s i escape llen elen)
  (declare (type (or simple-string character) escape)
           (type string s)
           (type fixnum i llen elen))
  (typecase escape
    (character (char= escape (schar s i)))
    (simple-string
     (when (<= (+ i elen) llen)
       (iter
         (declare (type fixnum eidx))
         (with eidx = 0)
         (always (char= (char escape eidx)
                        (char s (+ i eidx))))
         (incf eidx)
         (while (< eidx elen)))))))

(defvar *default-external-format* :default
  "the external format used for opening files")

(defun %in-stream (stream-or-string)
  (typecase stream-or-string
    (string (make-string-input-stream stream-or-string))
    (stream stream-or-string)
    (pathname (values (open stream-or-string :external-format *default-external-format*)
                      T))))

(defmacro with-csv-input-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%in-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))


(defun restartable-read-row (stream csv-reader)
  (handler-case
      (restart-case (read-csv-row stream :csv-reader csv-reader)
        (continue ()
          :report "skip reading this row and try again on the next"
          'do-next-iter)
        (filter (new-row)
          :report "supply a different row to use instead of this erroring csv-row"
          new-row))
    (end-of-file () 'finish-iteration)))

(iterate:defmacro-clause (for var in-csv input
                              &optional skipping-header skip-first-p
                              separator separator
                              quote quote
                              escaped-quote escaped-quote)
  "in-csv driver for iterate"
  (alexandria:with-unique-names (stream opened? skip csv-reader row)
    `(progn
      (with ,skip = ,skip-first-p)
      ;; can't bind values in a `with`, so listify and destructure
      (with (,stream ,opened?) = (multiple-value-list
                                  (%in-stream ,input)))
      (with *separator* = (or ,separator *separator*))
      (with *quote* = (or ,quote *quote*))
      (with *quote-escape* = (or ,escaped-quote *quote-escape*))
      (with ,csv-reader = (make-default-csv-reader))
      (finally-protected
       (when (and ,stream ,opened?)
         (close ,stream)))
      (for ,row = (restartable-read-row ,stream ,csv-reader))
      (when (or (and ,skip (first-iteration-p))
                (eql ,row 'do-next-iter))
        (next-iteration))
      (when (eql ,row 'finish-iteration)
        (finish))
      (for ,var = ,row)
      )))

(iterate:defmacro-clause (sampling expr &optional into var size size)
  "resevoir sample the input"
  (let ((sample (or var iterate::*result-var*)))
    (alexandria:with-unique-names (i sample-size sigil buffer row)
      `(progn
        (with ,sample)
        (with ,sample-size = (or ,size 100))
        (with ,buffer = (make-array ,sample-size :initial-element ',sigil))
        (with ,i = 0)
        (if (< ,i ,sample-size)
            (setf (aref ,buffer ,i) ,expr)
            (let ((r (random ,i)))
              (when (< r ,sample-size)
                (setf (aref ,buffer r) ,expr))))
        (incf ,i)
        (finally
         ;; convert our sample to a list, but only if we actually took the sample
         (when (plusp ,i)
           (setf ,sample
                 (iter (for ,row in-vector ,buffer)
                   (until (eq ,row ',sigil))
                   (collect ,row)))))))))

(defun read-csv-sample (stream-or-string sample-size
                        &key
                        row-fn map-fn
                        skip-first-p
                        ((:separator *separator*) *separator*)
                        ((:quote *quote*) *quote*)
                        ((:escape *quote-escape*) *quote-escape*)
                        ((:unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*)
                         *unquoted-empty-string-is-nil*)
                        ((:quoted-empty-string-is-nil *quoted-empty-string-is-nil*)
                         *quoted-empty-string-is-nil*)
                        ((:trim-outer-whitespace *trim-outer-whitespace*)
                         *trim-outer-whitespace*)
                        ((:newline *read-newline*) *read-newline*))

  (iter
    (for row in-csv stream-or-string skipping-header skip-first-p)
    (sampling row into sample size sample-size)
    (finally
     (return
       (iter (for row in sample)
         (when map-fn (setf row (funcall map-fn row)))
         (when row-fn (funcall row-fn sample))
         (collect row))))))



(defmacro do-csv ((row-var stream-or-pathname
                   &rest read-csv-keys)
                  &body body)
"row-var: a variable that is passed into _body_

stream-or-pathname: a stream or a pathname to read the CSV data from

read-csv-keys: keys and values passed to the _read-csv_ function

body: body of the macro"
  `(read-csv ,stream-or-pathname ,@read-csv-keys
    :row-fn #'(lambda (,row-var) ,@body)
    )
  )


(defun read-csv-row
    (stream-or-string
     &key
     csv-reader
     ((:separator *separator*) *separator*)
     ((:quote *quote*) *quote*)
     ((:escape *quote-escape*) *quote-escape*)
     ((:unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*)
      *unquoted-empty-string-is-nil*)
     ((:quoted-empty-string-is-nil *quoted-empty-string-is-nil*)
      *quoted-empty-string-is-nil*)
     ((:trim-outer-whitespace *trim-outer-whitespace*)
      *trim-outer-whitespace*)
     ((:newline *read-newline*) *read-newline*)
     ((:escape-mode *escape-mode*) *escape-mode*)
     )
  (read-csv-row-with-reader stream-or-string :csv-reader csv-reader))

(defun read-csv
    (stream-or-string
     &rest all-keys
     &key
     csv-reader row-fn map-fn data-map-fn sample skip-first-p 
     ((:separator *separator*) *separator*)
     ((:quote *quote*) *quote*)
     ((:escape *quote-escape*) *quote-escape*)
     ((:unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*)
      *unquoted-empty-string-is-nil*)
     ((:quoted-empty-string-is-nil *quoted-empty-string-is-nil*)
      *quoted-empty-string-is-nil*)
     ((:trim-outer-whitespace *trim-outer-whitespace*)
      *trim-outer-whitespace*)
     ((:newline *read-newline*) *read-newline*)
     ((:escape-mode *escape-mode*) *escape-mode*)
     )
  (declare (ignorable csv-reader data-map-fn))
  (if sample
      (read-csv-sample
       stream-or-string sample
       :row-fn row-fn :map-fn map-fn :skip-first-p skip-first-p)
      (let ((args (list* stream-or-string all-keys)))
      (apply #'read-csv-with-reader args))))

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
