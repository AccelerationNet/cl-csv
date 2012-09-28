(cl:in-package :cl-csv)

(defun get-data-table-from-csv (file &optional (has-column-names t) (munge-types t) sample
                                &aux (dt (make-instance 'data-table:data-table)))
  "Gets a data-table object representing the CSV"
  (cl-csv::with-csv-input-stream (in-stream file)
    (flet ((map-fn (row) (mapcar #'data-table::trim-and-nullify row)))
      (when has-column-names
        (setf (data-table:column-names dt) (map-fn (cl-csv::read-csv-row in-stream))))
      (setf (data-table:rows dt)
            (read-csv in-stream :map-fn #'map-fn :sample sample))
      (if munge-types
        (data-table:coerce-data-table-of-strings-to-types dt)
        (data-table::ensure-column-data-types dt))
      dt)))


(defun data-table-to-csv (dt &optional stream)
  "Write a datatable object out to csv"
  (write-csv (list* (data-table:column-names dt) (data-table:rows dt))
             :stream stream))

(defun get-data-table-from-csv-list
    (list &optional (has-column-names t) (munge-types t) sample
          &aux (dt (make-instance 'data-table:data-table)))
  "Create a data-table from the parsed csv as lisp lists"
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

