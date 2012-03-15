(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defun get-data-table-from-csv (file &optional (has-column-names t) (munge-types T) sample
                                &aux (dt (make-instance 'data-table:data-table)))
  (with-csv-input-stream (in-stream file)
    (flet ((map-fn (row) (mapcar #'data-table::trim-and-nullify row)))
      (when has-column-names
        (setf (data-table:column-names dt) (map-fn (read-csv-row in-stream))))
      (setf (data-table:rows dt)
            (read-csv in-stream :map-fn #'map-fn :sample sample))
      (when munge-types
        (data-table:coerce-data-table-of-strings-to-types dt))
      dt)))

(defun data-table-to-csv (dt &optional stream)
  (write-csv (list* (data-table:column-names dt) (data-table:rows dt))
             :stream stream))
