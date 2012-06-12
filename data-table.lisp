
(cl:defpackage #:cl-csv-data-table
  (:use :cl)
  (:use :cl-csv)
  (:export #:get-data-table-from-csv
           #:get-data-table-from-csv-list))

(cl:in-package #:cl-csv-data-table)

(defun get-data-table-from-csv (file &optional (has-column-names t) (munge-types T) sample
                                &aux (dt (make-instance 'data-table:data-table)))
  (cl-csv::with-csv-input-stream (in-stream file)
    (flet ((map-fn (row) (mapcar #'data-table::trim-and-nullify row)))
      (when has-column-names
        (setf (data-table:column-names dt) (map-fn (cl-csv::read-csv-row in-stream))))
      (setf (data-table:rows dt)
            (read-csv in-stream :map-fn #'map-fn :sample sample))
      (when munge-types
        (data-table:coerce-data-table-of-strings-to-types dt))
      dt)))

(defun get-data-table-from-csv-list
    (list &optional (has-column-names t) (munge-types T) sample
          &aux (dt (make-instance 'data-table:data-table)))
  (flet ((map-fn (row) (mapcar #'data-table::trim-and-nullify row)))
    (when has-column-names
      (setf (data-table:column-names dt) (map-fn (first list))))
    (setf (data-table:rows dt)
          (mapcar (lambda (x)
                    (if sample
                        (map-fn (subseq x 0 sample))
                        (map-fn x)))
                  (rest list)))
    (when munge-types
      (data-table:coerce-data-table-of-strings-to-types dt))
    dt))
