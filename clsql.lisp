(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defun import-from-csv (table-name &rest keys
                        &key file (data-table nil data-table-provided)
                        (schema "public") (should-have-serial-id "id")
                        excluded-columns)
  "Will make a best effor to create a table matching the csv's schema and then "
  (let* ((cl-interpol:*list-delimiter* ",")
         (*print-pretty* nil)
         (dt (or data-table (get-data-table-from-csv file)))
         (keys (copy-list keys)))
    (remf keys :file)
    (remf keys :data-table)

    (unless data-table-provided
      (coerce-data-table-of-strings-to-types dt))
    (when (and should-have-serial-id
               (member should-have-serial-id (column-names dt) :test #'string-equal))
      (error #?"This table already has an id column name `${should-have-serial-id}` Column! Perhaps you wish to turn off should-have-serial-id or assign it a different name?"))
    (apply #'ensure-table-for-data-table dt table-name keys)
    (import-data-table
     dt table-name :schema schema
     :excluded-columns excluded-columns)))

(defun serial-import-from-csv (table-name &key file
                               (schema "public") (downcase-columns T)
                               (progress-stream T) (progress-mod 5000))
  "Will make a best effor to create a table matching the csv's schema and then "
  (let ((cnt 0) columns)
    (flet ((row-fn (row)
             (if (= cnt 0)
                 (setf columns
                       (mapcar
                        (lambda (x) (trim-and-nullify
                                (if downcase-columns (nstring-downcase x) x)))
                        row))
                 (clsql-sys:insert-records
                  :into (clsql-sys:sql-expression :string #?"${schema}.${table-name}")
                  :attributes columns
                  :values row))
             (incf cnt)
             (when (zerop (mod progress progress-mod))
               (format progress-stream "Imported ~D rows~%" progress ))))))
  (with-input-from-file (s file)
    (read-csv s :row-fn #'row-fn)))