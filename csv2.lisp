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
   ))


(defmethod print-object ((o read-dispatch-table) s)
  "Print the auto-print-items for this instance."
  (print-unreadable-object (o s :type t :identity t)
    (ignore-errors
     (iter (for c in '(line-idx character-line-idx character-idx))
           (for v = (ignore-errors (funcall o c)))
           (when (and (typep v 'rational)
                      (not (typep v 'integer))
                      (not (typep v 'float)))
             (setf v (float v 0.0d0)))
           (when v (format s "~A:~S " c v))))
    (ignore-errors (format s "~S" (string (buffer o))))))

(defclass read-dispatch-table-entry ()
  ((delimiter :type (vector (or boolean character))
              :accessor delimiter :initarg :delimiter :initform nil)
   (didx :type fixnum :initform -1 :accessor didx :initarg :didx)
   (dlen :type fixnum :initform 0 :accessor dlen :initarg :dlen)
   (dlen-1 :type fixnum :initform -1 :accessor dlen-1 :initarg :dlen-1)
   (dispatch :type function :initform nil :accessor dispatch  :initarg :dispatch)
   ))

;; holds the states that the state machine events will drive
(defclass csv-reader (read-dispatch-table)
  ((rows :initform
         (make-array 10
                     :element-type '(vector string)
                     :initial-element #()
                     :adjustable t :fill-pointer 0)
         :accessor rows :initarg rows)
   (line-data :initform
              (make-array 10
                          :element-type 'string
                          :initial-element ""
                          :adjustable t :fill-pointer 0)
              :accessor line-data)
   (reading-quoted? :initform () :accessor reading-quoted?)
   (after-quoted?  :initform () :accessor after-quoted?)))

(defmethod reading-quoted (csv-reader c  &key table-entry)
  (declare (ignorable table-entry))
  ;; skips collection of quote characters
  (when (and (not (reading-quoted? csv-reader))
             (plusp (fill-pointer (buffer csv-reader))))
    (csv-parse-error "we are reading non quoted csv data and found a quote at~%~A~%~A"
                     csv-reader c))
  (setf (reading-quoted? csv-reader)
        (not (reading-quoted? csv-reader)))
  (when (not (reading-quoted? csv-reader))
    (setf (after-quoted? csv-reader) t)))

(defun last-item (buff)
  (let ((idx (- (fill-pointer buff) 1)))
    (if (plusp idx)
        (aref buff idx)
        nil)))

(defun (setf last-item) (new buff)
  (let ((idx (- (fill-pointer buff) 1)))
    (setf (aref buff idx) new)))

(defmethod reading-escaped (csv-reader c &key table-entry)
  (declare (ignorable table-entry))
  (case *escape-mode*
    (:quote
     (cond
       ((reading-quoted? csv-reader)
        (vector-push-extend *quote* (buffer csv-reader))
        ;; read an empty data item, let it ride
        (setf (reading-quoted? csv-reader) nil)
        (vector-push-extend "" (line-data csv-reader)))
       (t
        ;; roll back the most recent collection
        ;; and put buffer back
        (let ((s (last-item (line-data csv-reader))))
          (setf (reading-quoted? csv-reader) t)
          (setf (fill-pointer (buffer csv-reader))
                (length s))
          (vector-push-extend c (buffer csv-reader))
          (decf (fill-pointer (line-data csv-reader)))))))
    (:following
     ;; replace the previous character with
     ;; the char escaped
     (setf (last-item (buffer csv-reader)) c))))

(defun collect-line-data (csv-reader)
  (when *trim-outer-whitespace*
    (iter
     (while (and (white-space? (last-item (buffer csv-reader)))
                 (plusp (fill-pointer (buffer csv-reader)))))
     (decf (fill-pointer (buffer csv-reader)))))
  (let ((d (copy-seq (string (buffer csv-reader)))))
    (csv-data-read d)
    (vector-push-extend d (line-data csv-reader)))
  (setf (fill-pointer (buffer csv-reader))  0))

(defun collect-row-data (csv-reader)
  (collect-line-data csv-reader)
  (let ((row (copy-seq (line-data csv-reader))))
    (csv-row-read row)
    (vector-push-extend row (rows csv-reader)))
  (setf (character-line-idx csv-reader) 0)
  (setf (line-data csv-reader)
        (make-array 10
                    :element-type 'string
                    :initial-element ""
                    :adjustable t :fill-pointer 0)))

(defmethod reading-separator (csv-reader c &key table-entry)
  (declare (ignorable table-entry))
  (cond
    ((not (reading-quoted? csv-reader))
     (setf (after-quoted? csv-reader) nil)
     (collect-line-data csv-reader)
     t)
    (t nil)))

(defmethod reading-newline (csv-reader c &key table-entry)
  (declare (ignorable table-entry))
  (incf (line-idx csv-reader))
  (cond
    ((not (reading-quoted? csv-reader))
     (collect-row-data csv-reader)
     t)
    (t (vector-push-extend c (buffer csv-reader)))))

(defun reading-character (csv-reader c &key table-entry)
  (declare (ignorable table-entry))
  (cond
    ((and *trim-outer-whitespace*
          (white-space? c)
          (zerop (fill-pointer (buffer csv-reader))))
     ;; skip storing prefixing whitespace
     T)
    ((and (after-quoted? csv-reader)
          (not (white-space? c)))
     (csv-parse-error "non whitespace after quoted data ~a ~a"
                      csv-reader c))
    (t
     ;; store the character
     (vector-push-extend c (buffer csv-reader))
     t)))

(defun make-default-csv-dispatch-table ()
  "Creates the default csv dispatch table, for each item"
  (make-instance
   'csv-reader
   :entries
   (vector
    (if (equal *escape-mode* :following)
        (make-table-entry (vector *quote-escape* t) #'reading-escaped)
        (make-table-entry (vector *quote* *quote*) #'reading-escaped))

    (make-table-entry *quote* #'reading-quoted)
    (make-table-entry *separator* #'reading-separator)
    (make-table-entry (vector #\return #\newline) #'reading-newline)
    (make-table-entry (vector #\return #\return) #'reading-newline)
    (make-table-entry (vector #\newline) #'reading-newline)
    (make-table-entry t #'reading-character))))

(defun make-table-entry (delimiter dispatch)
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
  (setf (didx te) -1))

(defun check-table-entry (te c)
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

(defun check-and-distpatch (table c &aux dispatched?)
  (iter (for entry in-vector (entries table))
        (when (typep entry 'read-dispatch-table-entry)
          (when (check-table-entry entry c)
            (setf dispatched? t)
            (funcall (dispatch entry) table c :table-entry entry))))
  dispatched?)

(defun read-with-dispatch-table (table stream)
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
  (when (reading-quoted? table)
    (csv-parse-error "End of file while collecting quoted item: ~A" table))
  (when (or (plusp (fill-pointer (buffer table)))
            (plusp (fill-pointer (line-data table))))
    (collect-row-data table)))

(defun read-csv-with-table (stream-or-string &key table)
  (unless table
    (setf table (make-default-csv-dispatch-table)))
  (with-csv-input-stream (in-stream stream-or-string)
    (read-with-dispatch-table table in-stream)))

(defun read-csv-row-with-table (stream-or-string &key table)
  (unless table
    (setf table (make-default-csv-dispatch-table)))
  (with-csv-input-stream (in-stream stream-or-string)
    (let ((*enable-signals* t))
      (handler-bind ((csv-row-read
                       (lambda (c)
                         (adwutils:spy-break table (row c))
                         (return-from read-csv-row-with-table
                           (row c)))))
        (read-with-dispatch-table table in-stream)))))
