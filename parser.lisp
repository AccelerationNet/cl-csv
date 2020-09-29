(in-package :cl-csv)

(defclass read-dispatch-table ()
  ((parse-stream :accessor parse-stream :initarg :parse-stream :initform nil)
   (buffer
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
  ((delimiter :type (or (vector (or boolean character)) null)
              :accessor delimiter :initarg :delimiter :initform nil)
   (didx :type fixnum :initform -1 :accessor didx :initarg :didx)
   (dlen :type fixnum :initform 0 :accessor dlen :initarg :dlen)
   (dlen-1 :type fixnum :initform -1 :accessor dlen-1 :initarg :dlen-1)
   (dispatch :type (or function null) :initform nil :accessor dispatch  :initarg :dispatch)
   )
  (:documentation "When a certain delimiter is matched it will call a certain function
    T matches anything
    create these with make-table-entry"))

(defun make-table-entry (delimiter dispatch
                         &key (class 'read-dispatch-table-entry))
  "Creates a table entry ensuring everything has the correct
   types and values"
  (let* ((d (if (eql delimiter t)
                #(t)
                (etypecase delimiter
                  (null (return-from make-table-entry nil))
                  (character (vector delimiter))
                  (string delimiter)
                  ((or simple-vector vector simple-array array) delimiter)
                  (list (apply #'vector delimiter)))))
         (te (make-instance
              class
              :delimiter d
              :dispatch dispatch
              :dlen (length d)
              :dlen-1 (- (length d) 1))))
    te))

(defun reset-table-entry (te)
  "resets the entry state when it doesnt match"
  (setf (didx te) -1))

(defmethod check-table-entry (table entry c)
  "Given the next character in a stream check if the table entry matches
   reset if it matches fully or doesnt match"
  (declare (ignore table))
  (incf (didx entry))
  (let ((de-char (aref (delimiter entry) (didx entry))))
    (cond
      ((or (eql t de-char)
           (char= c de-char))
       (when (eql (didx entry) (dlen-1 entry))
         (reset-table-entry entry)
         t))
      (t
       (reset-table-entry entry)
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
   (after-quoted? :initform () :accessor after-quoted?)
   (row-fn :initform () :accessor row-fn :initarg :row-fn)
   (map-fn :initform () :accessor map-fn :initarg :map-fn)
   (data-map-fn :initform 'map-empty-string-to-nil :accessor data-map-fn :initarg :data-map-fn)
   (skip-row? :initform nil :initarg :skip-row? :accessor skip-row?)
   )
  (:documentation "the state of the csv reader, which is also is a read table"))

(defun map-empty-string-to-nil (data &key csv-reader &allow-other-keys)
  (if (and
       (stringp data)            ;; other filters  /mapping could have happend
       (zerop (length data))
       (or (and (after-quoted? csv-reader)
                *quoted-empty-string-is-nil*)
           (and (not (after-quoted? csv-reader))
                *unquoted-empty-string-is-nil*)))
      nil
      data))


(defun last-item (buff &key (n 1))
  (let ((idx (- (fill-pointer buff) n)))
    (if (plusp idx)
        (aref buff idx)
        nil)))

(defun (setf last-item) (new buff)
  (let ((idx (- (fill-pointer buff) 1)))
    (setf (aref buff idx) new)))

(defun %next-char (reader)
  (incf (character-idx reader))
  (incf (character-line-idx reader))
  (read-char (parse-stream reader) nil nil))

(defmethod reading-quoted-or-escaped (csv-reader c  &key table-entry)
  "Method to handle reading a quote or a pair of quotes"
  (declare (ignorable table-entry))
  (assert (and (eql *escape-mode* :quote)
               (or (null *quote-escape*)
                   (%escape-is-double-quote))))
  (cond
    ;; we finished reading quoted and got more
    ((after-quoted? csv-reader)
     (csv-parse-error
      "we are reading non quoted csv data and found a quote at~%~A~%~A"
      csv-reader c))

    ;; not reading quoted so start to
     ((not (reading-quoted? csv-reader))
      (setf (reading-quoted? csv-reader) t))

     ;; if we are reading quoted, read a quote and the next char is a quote
     ;; store the quote and skip a char

     ((and (reading-quoted? csv-reader)
           (equal *quote* (peek-char nil (parse-stream csv-reader) nil nil)))
      (%next-char csv-reader)
      (vector-push-extend c (buffer csv-reader)))

     ((reading-quoted? csv-reader)
      (setf (after-quoted? csv-reader) t
            (reading-quoted? csv-reader) nil))
     )
  t)

(defmethod reading-quoted (csv-reader c  &key table-entry)
  "Method to handle reading a quote
   NB: this interacts wierdly with escape-mode :quote "
  (declare (ignorable table-entry))


  (when (and (not (reading-quoted? csv-reader))
             (plusp (fill-pointer (buffer csv-reader))))
    ;; TODO: this could probably be removed and just let that fly
    (csv-parse-error "we are reading non quoted csv data and found a quote at~%~A~%~A"
                     csv-reader c))

    (setf (reading-quoted? csv-reader) (not (reading-quoted? csv-reader)))

  (when (not (reading-quoted? csv-reader))
    (setf (after-quoted? csv-reader) t))
  t)

(defmethod reading-following-escaped (csv-reader c &key table-entry)
  "We read an escape sequence and need to handle storing
   the escaped character"
  (declare (ignorable table-entry))
  (ecase *escape-mode*
    (:following
     ;; replace the previous character with
     ;; the char escaped
     (setf (last-item (buffer csv-reader)) c)
     ))
  t)

(defmethod reading-escaped (csv-reader c &key table-entry)
  "We read an escape sequence and need to handle storing
   the escaped character"
  (drop-delimiter-chars csv-reader table-entry)
  (vector-push-extend *quote* (buffer csv-reader))
  t)

(defun %trim-datum (csv-reader &aux (b (buffer csv-reader)))
  (when (and (not (after-quoted? csv-reader))
             (not (reading-quoted? csv-reader))
             *trim-outer-whitespace*)
    (iter
      (while (and (white-space? (last-item b))
                  (plusp (fill-pointer b))))
      (decf (fill-pointer b))))
  b
  )

(defun collect-datum (csv-reader
                      &aux (data-map-fn (data-map-fn csv-reader)))
  (let ((d (copy-seq (%trim-datum csv-reader))))
    (setf d (csv-data-read d :csv-reader csv-reader))
    (when data-map-fn
      (setf d (funcall data-map-fn d :csv-reader csv-reader)))
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
  (iter (for i from (- (length (delimiter entry)) 2) downto 0)
        (while (eql (last-item (buffer table))
                    (aref (delimiter entry) i)))
        (decf (fill-pointer (buffer table)))
        ))

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

(defun %escape-is-double-quote
    (&aux
     (x (typecase *quote*
          ((or sequence string) (concatenate 'vector *quote* *quote*))
          (character (vector *quote* *quote*)))))
  (typecase *quote-escape*
    ((or null character) nil)
    (sequence
     (equalp x *quote-escape*))
  ))

(defun make-default-csv-reader ()
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
      (when *quote*
        (cond
          ;; Escape is two adjacent quotes in quoted data
          ((and (eql *escape-mode* :quote)
                (or (null *quote-escape*)
                    (%escape-is-double-quote)))
           (make-table-entry *quote* #'reading-quoted-or-escaped))
          ;; escape is a literal sequence of chars that mean quote
          ((eql *escape-mode* :quote)
            (list (make-table-entry *quote-escape* #'reading-escaped)
                  (make-table-entry *quote* #'reading-quoted)))
          ;; escape is a backslash followed by the char being escaped
          ((eql *escape-mode* :following)
           (list
            (make-table-entry
             (etypecase *quote-escape*
               (null (vector #\\ t))
               (string (apply #'vector (append (coerce *quote-escape* 'list) '(t))))
               (character (vector *quote-escape* t)))
             #'reading-following-escaped)
            (make-table-entry *quote* #'reading-quoted)))))

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
          (when (check-table-entry table entry c)
            (funcall (dispatch entry) table c :table-entry entry)
            (return t)))))

(defun read-with-dispatch-table (table stream &aux (read-cnt 0))
  "A generic function for processing all the characters of a stream until
   a match arises and collecting that data as it goes"
  (iter
    (setf (parse-stream table) stream)
    (for c = (%next-char table))
    (while c)
    (incf read-cnt)
    (cond
      ((check-and-distpatch table c)
       t                            ;; it handles what to do with the data etc
       )
      (t
       ;; didnt dispatch so store
       (vector-push-extend c (buffer table)))))
  (when (zerop read-cnt)
    (error (make-condition 'end-of-file :stream stream)))
  (when (reading-quoted? table)
    (restart-case
        (csv-parse-error "End of file while collecting quoted item: ~A" table)
      (finish-item ())))
  (when (or (plusp (fill-pointer (buffer table)))
            (plusp (fill-pointer (line-data table))))
    (collect-row-data table)))

(defun read-csv-with-reader (stream-or-string
                             &key csv-reader
                             (row-fn nil row-fn-p)
                             (map-fn nil map-fn-p)
                             (data-map-fn nil data-map-fn-p)
                             skip-first-p
                             &allow-other-keys)
  "Read a whole csv from the input"
  (unless csv-reader
    (setf csv-reader (make-default-csv-reader)))
  (when row-fn-p (setf (row-fn csv-reader) row-fn))
  (when map-fn-p (setf (map-fn csv-reader) map-fn))
  (when data-map-fn-p (setf (data-map-fn csv-reader) data-map-fn))
  (setf (skip-row? csv-reader) skip-first-p)
  (handler-case
      (with-csv-input-stream (in-stream stream-or-string)
        (read-with-dispatch-table csv-reader in-stream)
        (coerce (rows csv-reader) 'list))
    (end-of-file (c)
      ;; this is signaled if we read an empty csv
      ;; it needs to happend for read-csv-row to work
      ;; with iterate, but doesnt need to happen in this fn
      (declare (ignore c))
      (coerce (rows csv-reader) 'list))))

(defun read-csv-row-with-reader (stream-or-string
                                &key csv-reader
                                (map-fn nil map-fn-p)
                                (data-map-fn nil data-map-fn-p)
                                &allow-other-keys)
  "Read a row of csv from the input"
  (flet ((return-row (it)
           (return-from read-csv-row-with-reader it)))
    (unless csv-reader
      (setf csv-reader (make-default-csv-reader)))
    (when map-fn-p (setf (map-fn csv-reader) map-fn))
    (when data-map-fn-p (setf (data-map-fn csv-reader) data-map-fn))
    (setf (row-fn csv-reader) #'return-row)
    (with-csv-input-stream (in-stream stream-or-string)
      (read-with-dispatch-table csv-reader in-stream))))
