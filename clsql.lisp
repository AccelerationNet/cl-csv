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
  (with-input-from-file (s file)
    (let* ((cl-interpol:*list-delimiter* ",")
           (*print-pretty* nil)
           (columns (iter (for x in (cl-ppcre:split
                                     #?"(\"|\\s|,)+"
                                     (dos-safe-read-line s) :omit-unmatched-p T))
                          ;; the first column might be empty string due to leading
                          ;; single quote
                          (awhen (trim-and-nullify
                                  (if downcase-columns (nstring-downcase x) x))
                            (collect (clsql-sys:sql-expression :attribute it)))

                          )))
      (iter (for line in-stream s using #'dos-safe-read-line)
            (for data = (parse-csv-string line))
            (counting line into progress )
            (clsql-sys:insert-records
             :into (clsql-sys:sql-expression :string #?"${schema}.${table-name}")
             :attributes columns
             :values data)
            (when (zerop (mod progress progress-mod))
              (format progress-stream "Imported ~D rows~%" progress )))
      )))