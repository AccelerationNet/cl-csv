(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defmethod format-csv-value ((val clsql-sys:date))
  (clsql-helper:print-nullable-date val))

(defmethod format-csv-value ((val clsql-sys:wall-time))
  (clsql-helper:print-nullable-datetime val))

(defun export-query ( sql &key stream path)
  (with-csv-output-stream (s (or stream path))
    (multiple-value-bind (rows cols)
        (clsql:query sql :flatp T)
      (write-csv-row cols :stream s)
      (write-csv rows :stream s))))

(defun import-from-csv (table-name &rest keys
                        &key file (data-table nil data-table-provided)
                        (schema "public") (should-have-serial-id "id")
                        excluded-columns row-fn)
  "Will make a best effor to create a table matching the csv's schema and then

   row-fn (data-row schema table columns)
          allows you to take actions on a row  (before insert).
          returning false will prevent the default insert
  "
  (declare (ignorable schema)) ;; we pass it in keys
  (let* ((cl-interpol:*list-delimiter* ",")
         (*print-pretty* nil)
         (dt (or data-table (get-data-table-from-csv file)))
         (keys (copy-list keys)))
    (remf keys :file)
    (remf keys :row-fn)
    (remf keys :data-table)

    (unless data-table-provided
      (data-table:coerce-data-table-of-strings-to-types dt))
    (when (and should-have-serial-id
               (member should-have-serial-id (data-table:column-names dt) :test #'string-equal))
      (error #?"This table already has an id column name `${should-have-serial-id}` Column! Perhaps you wish to turn off should-have-serial-id or assign it a different name?"))
    (apply #'data-table:ensure-table-for-data-table dt table-name keys)
    (data-table:import-data-table dt table-name excluded-columns :row-fn row-fn)))

(defun serial-import-from-csv (table-name &key file
                               (schema "public") (downcase-columns T)
                               (progress-stream T) (progress-mod 5000))
  "Will make a best effor to create a table matching the csv's schema and then "
  (let ((cnt 0) columns)
    (flet ((row-fn (row)
             (if (= cnt 0)
                 (setf columns
                       (mapcar
                        (lambda (x) (clsql-helper::trim-and-nullify
                                (if downcase-columns (nstring-downcase x) x)))
                        row))
                 (clsql-sys:insert-records
                  :into (clsql-sys:sql-expression :string #?"${schema}.${table-name}")
                  :attributes columns
                  :values row))
             (incf cnt)
             (when (zerop (mod cnt progress-mod))
               (format progress-stream "Imported ~D rows~%" cnt ))))
      (read-csv file :row-fn #'row-fn))))