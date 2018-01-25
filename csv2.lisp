(in-package :cl-csv)

(defclass read-dispatch-table ()
  ((buffer
    :accessor buffer
    :initarg :buffer
    :initform (make-array *buffer-size*
                          :element-type 'character
                          :initial-element #\null
                          :adjustable t :fill-pointer 0))
   (entries :initform nil :initarg :entries :accessor entries )
   (line-idx :initform 0 :initarg :line-idx :accessor line-idx)
   (character-line-idx
    :initform 0 :initarg :character-line-idx :accessor character-line-idx)
   (character-idx :initform 0 :initarg :character-idx :accessor character-idx)
   )
  (:documentation " A stream parser that collects characters
and when a certain delimiter is matched will call a certain function.
These delimiter / function pairs are read-dispatch table entries
It contains all the state for the parse process
See: csv-reader "))


(defmethod print-object ((o read-dispatch-table) s)
  "Print the auto-print-items for this instance."
  (print-unreadable-object (o s :type t :identity t)
     (iter (for slt in '(line-idx character-line-idx character-idx))
           (for v = (funcall slt o))
           (when v (format s "~A:~S " slt v)))
    (ignore-errors (format s "~S" (string (buffer o))))))

(defclass read-dispatch-table-entry ()
  ((delimiter :type (vector (or boolean character))
              :accessor delimiter :initarg :delimiter :initform nil)
   (didx :type fixnum :initform -1 :accessor didx :initarg :didx)
   (dlen :type fixnum :initform 0 :accessor dlen :initarg :dlen)
   (dlen-1 :type fixnum :initform -1 :accessor dlen-1 :initarg :dlen-1)
   (dispatch :type function :initform nil :accessor dispatch  :initarg :dispatch)
   )
  (:documentation "When a certain delimiter is matched it will call a certain function
    T matches anything
    create these with make-table-entry"))

(defun make-table-entry (delimiter dispatch)
  "Creates a table entry ensuring everything has the correct
   types and values"
  (let* ((d (if (eql delimiter t)
                #(t)
                (etypecase delimiter
                  (character (vector delimiter))
                  (string delimiter)
                  ((or simple-vector vector simple-array array) delimiter)
                  (list (apply #'vector delimiter)))))
         (te (make-instance
              'read-dispatch-table-entry
              :delimiter d
              :dispatch dispatch
              :dlen (length d)
              :dlen-1 (- (length d) 1))))
    te))

(defun reset-table-entry (te)
  "resets the entry state when it doesnt match"
  (setf (didx te) -1))

(defun check-table-entry (te c)
  "Given the next character in a stream check if the table entry matches
   reset if it matches fully or doesnt match"
  (incf (didx te))
  (let ((de-char (aref (delimiter te) (didx te))))
    (cond
      ((or (eql t de-char)
           (char= c de-char))
       (when (eql (didx te) (dlen-1 te))
         (reset-table-entry te)
         t))
      (t
       (reset-table-entry te)
       nil))))

;; holds the states that the state machine events will drive
(defclass csv-reader (read-dispatch-table)
  ((rows :initform
         (make-array 10
                     :element-type 'list
                     :initial-element ()
                     :adjustable t :fill-pointer 0)
         :accessor rows :initarg rows)
   (line-data :initform
              (make-array 10
                          :element-type 'string
                          :initial-element ""
                          :adjustable t :fill-pointer 0)
              :accessor line-data)
   (reading-quoted? :initform () :accessor reading-quoted?)
   (after-quoted?  :initform () :accessor after-quoted?)
   (row-fn :initform () :accessor row-fn :initarg :row-fn)
   (map-fn :initform () :accessor map-fn :initarg :map-fn)
   (skip-row? :initform nil :initarg :skip-row? :accessor skip-row?))
  (:documentation "the state of the csv reader, which is also is a read table"))


(defmethod reading-quoted (csv-reader c  &key table-entry)
  "Method to handle reading a quote
   NB: this interacts wierdly with escape-mode :quote "
  (declare (ignorable table-entry))
  ;; skips collection of quote characters
  (when (and (not (reading-quoted? csv-reader))
             (plusp (fill-pointer (buffer csv-reader))))
    (csv-parse-error "we are reading non quoted csv data and found a quote at~%~A~%~A"
                     csv-reader c))
  (setf (reading-quoted? csv-reader)
        (not (reading-quoted? csv-reader)))
  (when (not (reading-quoted? csv-reader))
    (setf (after-quoted? csv-reader) t))
  t)

(defun last-item (buff)
  (let ((idx (- (fill-pointer buff) 1)))
    (if (plusp idx)
        (aref buff idx)
        nil)))

(defun (setf last-item) (new buff)
  (let ((idx (- (fill-pointer buff) 1)))
    (setf (aref buff idx) new)))

(defmethod reading-escaped (csv-reader c &key table-entry)
  "We read an escape sequence and need to handle storing
   the escaped character"
  (declare (ignorable table-entry))
  (case *escape-mode*
    (:quote
     (cond
       ;; read an empty data item, let it ride
       ((zerop (fill-pointer (buffer csv-reader)))
        (setf (reading-quoted? csv-reader) nil
              (after-quoted? csv-reader) t))
       (t
        ;; we got an escape instead of an end quote
        (setf (reading-quoted? csv-reader) t
              (after-quoted? csv-reader) nil)
        (vector-push-extend c (buffer csv-reader)))))
    (:following
     ;; replace the previous character with
     ;; the char escaped
     (setf (last-item (buffer csv-reader)) c)
     ))
  t)

(defun collect-datum (csv-reader)
  (when (and (not (after-quoted? csv-reader))
             (not (reading-quoted? csv-reader))
             *trim-outer-whitespace*)
    (iter
     (while (and (white-space? (last-item (buffer csv-reader)))
                 (plusp (fill-pointer (buffer csv-reader)))))
     (decf (fill-pointer (buffer csv-reader)))))

  (let ((d (copy-seq (string (buffer csv-reader)))))
    (when (and (zerop (length d))
               (or (and (after-quoted? csv-reader)
                        *quoted-empty-string-is-nil*)
                   (and (not (after-quoted? csv-reader))
                        *unquoted-empty-string-is-nil*)))
      (setf d nil))
    (setf d (csv-data-read d :csv-reader csv-reader))
    (vector-push-extend d (line-data csv-reader)))
  (setf (fill-pointer (buffer csv-reader)) 0
        (reading-quoted? csv-reader) nil
        (after-quoted? csv-reader) nil))

(defun collect-row-data (csv-reader
                         &aux (map-fn (map-fn csv-reader))
                         (row-fn (row-fn csv-reader)))
  (collect-datum csv-reader)
  (let ((row (coerce (line-data csv-reader) 'list)))
    (setf (fill-pointer (line-data csv-reader)) 0)
    (setf row (csv-row-read row :csv-reader csv-reader))
    (when map-fn
      (setf row (funcall map-fn row)))
    (if (skip-row? csv-reader)
        (setf (skip-row? csv-reader) nil) ;; we skipped
        (if row-fn
            (funcall row-fn row)
            (vector-push-extend row (rows csv-reader))))))

(defun drop-delimiter-chars (table entry)
  "This backs up the buffer till the delimiter is not in it
   we call this without having adding the character we just got
   that dispatched"
  (dotimes (i (- (length (delimiter entry)) 1))
    (decf (fill-pointer (buffer table)))))

(defmethod reading-separator (csv-reader c &key table-entry)
  "We got the data separator character which will be handled
   differently based on if we are in quoted data or not"
  (cond
    ((not (reading-quoted? csv-reader))
     ;; rewind to before delim
     (drop-delimiter-chars csv-reader table-entry)
     (collect-datum csv-reader)
     t)
    (t (vector-push-extend c (buffer csv-reader)))))

(defmethod reading-newline (csv-reader c &key table-entry)
   "We got the newline character which will be handled
   differently based on if we are in quoted data or not"
  (declare (ignorable table-entry))
  (incf (line-idx csv-reader))
  (setf (character-line-idx csv-reader) 0)
  (cond
    ((not (reading-quoted? csv-reader))
     ;; rewind to before delim
     (drop-delimiter-chars csv-reader table-entry)
     (collect-row-data csv-reader)
     t)
    (t (vector-push-extend c (buffer csv-reader))
       t )))

(defun reading-character (csv-reader c &key table-entry)
  "We read a random character that was not otherwise dispatched on"
  (declare (ignorable table-entry))
  (cond
    ((and *trim-outer-whitespace*
          (not (reading-quoted? csv-reader))
          (white-space? c)
          (or (zerop (fill-pointer (buffer csv-reader)))
              (after-quoted? csv-reader)))
     ;; skip storing whitespace before or after
     T)
    ((after-quoted? csv-reader)
     (cond
       ((not (white-space? c))
        (csv-parse-error "non whitespace after quoted data ~a ~a"
                         csv-reader c))
       ((not *trim-outer-whitespace*)
        (csv-parse-error "whitespace after quoted data thats not supposed to be trimmed ~a ~a"
                         csv-reader c))
       (t (vector-push-extend c (buffer csv-reader)))))
    (t
     ;; store the character
     (vector-push-extend c (buffer csv-reader))
     t)))

(defun make-default-csv-dispatch-table ()
  "Creates the default csv dispatch table
   This can usually be fully changed simply by tweaking the special variables
   defined in vars. You will need to reinstantiate this object when you change those variables
   (which is what happens by default)"
  (make-instance
   'csv-reader
   :entries
   (apply
    #'vector
    (alexandria:flatten
     (list
      (if (equal *escape-mode* :following)
          (make-table-entry (vector *quote-escape* t) #'reading-escaped)
          (make-table-entry (vector *quote* *quote*) #'reading-escaped))
      (make-table-entry *quote* #'reading-quoted)
      (make-table-entry *separator* #'reading-separator)

      (if (member *read-newline* '(t nil) :test #'equalp)
          (list
           (make-table-entry (vector #\return #\newline) #'reading-newline)
           (make-table-entry (vector #\return #\return) #'reading-newline)
           (make-table-entry (vector #\newline) #'reading-newline))
          (make-table-entry *read-newline* #'reading-newline))
      (make-table-entry t #'reading-character)
      )))))



(defun check-and-distpatch (table c)
  "Check all the entries in a read-dispatch-table to find a match
   if it matches, call the function with the table character and entry"
  (iter (for entry in-vector (entries table))
        (when (typep entry 'read-dispatch-table-entry)
          (when (check-table-entry entry c)
            (funcall (dispatch entry) table c :table-entry entry)
            (return t)))))

(defun read-with-dispatch-table (table stream)
  "A generic function for processing all the characters of a stream until
   a match arises and collecting that data as it goes"
  (iter (for c = (read-char stream nil nil))
        (while c)
        (incf (character-line-idx table))
        (incf (character-idx table))
        (cond
          ((check-and-distpatch table c)
           t ;; it handles what to do with the data etc
           )
          (t
           ;; didnt dispatch so store
           (vector-push-extend c (buffer table)))))
  (when (zerop (character-idx table))
    (error (make-condition 'end-of-file :stream stream)))
  (when (reading-quoted? table)
    (restart-case
        (csv-parse-error "End of file while collecting quoted item: ~A" table)
      (finish-item ())))
  (when (or (plusp (fill-pointer (buffer table)))
            (plusp (fill-pointer (line-data table))))
    (collect-row-data table)))

(defun read-csv-with-table (stream-or-string &key table row-fn map-fn skip-first-p)
  "Read a whole csv from the input"
  (unless table
    (setf table (make-default-csv-dispatch-table)))
  (setf (row-fn table) row-fn)
  (setf (map-fn table) map-fn)
  (setf (skip-row? table) skip-first-p)
  (with-csv-input-stream (in-stream stream-or-string)
    (read-with-dispatch-table table in-stream)
    (coerce (rows table) 'list)))

(defun read-csv-row-with-table (stream-or-string &key table)
  "Read a row of csv from the input"
  (flet ((return-row (it)
           (return-from read-csv-row-with-table it)))
    (unless table
      (setf table (make-default-csv-dispatch-table)))
    (setf (row-fn table) #'return-row)
    (with-csv-input-stream (in-stream stream-or-string)
      (read-with-dispatch-table table in-stream))))
