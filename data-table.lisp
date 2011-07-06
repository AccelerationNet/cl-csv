(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defun get-data-table-from-csv (file &optional (has-column-names t) (munge-types T))
  (let* ((dt (make-instance 'data-table:data-table))
         (data (read-csv
                file
                :row-fn (lambda (row) (mapcar #'data-table::trim-and-nullify
                                         row)))))
    (if has-column-names
        (setf
         (data-table:column-names dt) (first data)
         (data-table:rows dt) (rest data))
        (setf (data-table:rows dt) data))
    (when munge-types
      (data-table:coerce-data-table-of-strings-to-types dt))
    dt))