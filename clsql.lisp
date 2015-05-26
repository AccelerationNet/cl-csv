(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defun exec (sql)
  (clsql-sys:execute-command sql))

(defmethod format-csv-value ((val clsql-sys:date))
  (clsql-helper:print-nullable-date val))

(defmethod format-csv-value ((val clsql-sys:wall-time))
  (clsql-helper:print-nullable-datetime val))

(defun export-query ( sql &key stream path)
  (with-csv-output-stream (s (or stream path))
    (multiple-value-bind (rows cols)
        (clsql:query sql :flatp t)
      (write-csv-row cols :stream s)
      (write-csv rows :stream s))))

(defun import-from-csv (table-name &rest keys
                        &key file data-table (sample-size 1000)
                        schema (should-have-serial-id "id")
                        excluded-columns row-fn
                        (log-fn #'(lambda (&rest args) (declare (ignore args))))
                        (log-frequency 1000)
                        &aux (cl-interpol:*list-delimiter* ",")
                        (*print-pretty* nil))
  "Will make a best effor to create a table matching the csv's schema and then

   row-fn (data-row schema table columns)
          allows you to take actions on a row  (before insert).
          returning false will prevent the default insert

   log-fn (msg &rest args)
          function to log progress

   log-frequency : how frequent (measured in rows) to log progress

  "
;;; TODO: accept column names from params
;;; TODO: figure out how to process files without columns in the first row
;;; TODO: figure out how to override type guesses
;;; TODO: check if the table already exists and skip the guessing
  (funcall log-fn "Starting import ~a" table-name)
  (let ((dt (or data-table (get-data-table-from-csv file t t sample-size)))
        (keys (copy-list keys)))
    ;; we are putting this in a database, we cannot have empty column names
    (setf
     (data-table:column-names dt)
     (iter
       (with i = 0)
       (for c in (data-table:column-names dt))
       (collect (or c #?"anon_${ (incf i) }"))))
    (dolist (k '(:file :data-table :row-fn :sample-size :log-fn :log-frequency))
      (remf keys k))
    (funcall log-fn "CSV scanned for type information")
    (when (and should-have-serial-id
               (member should-have-serial-id (data-table:column-names dt) :test #'string-equal))
      (error #?"This table already has an id column name `${should-have-serial-id}` Column! Perhaps you wish to turn off should-have-serial-id or assign it a different name?"))
    (apply #'data-table:ensure-table-for-data-table dt table-name keys)
    (funcall log-fn "Created table, starting import")
    (let ((start-time (get-universal-time)))
      (flet ((log-progress (row-num &optional (msg "Processing row"))
               (let ((elapsed (- (get-universal-time) start-time)))
                 (funcall log-fn "~a ~a. ~ds elapsed (~,2f rows/sec) "
                          msg row-num elapsed
                          (if (zerop elapsed) "Inf"
                              (/ row-num elapsed))))))
        (iter
          (with importer = (data-table::make-row-importer
                            dt table-name :schema schema :excluded-columns excluded-columns
                            :row-fn row-fn))
          (for row in-csv file SKIPPING-HEADER T)
          (for row-num from 1)
          (funcall importer row)
          (when (zerop (mod row-num log-frequency)) (log-progress row-num))
          (finally (log-progress row-num "Finished, total processed: ")))))))

(defun serial-import-from-csv (table-name
                               &key file
                               (column-names :first-row)
                               (schema "public") (column-transform
                                                  #'data-table::english->postgres)
                               (progress-stream t) (progress-mod 5000)
                               (data-munger (lambda (row)
                                              (mapcar #'clsql-helper:format-value-for-database
                                                      row)))
                               (log (lambda (msg &rest args)
                                      (apply #'format progress-stream msg args)))
                               (on-error 'continue-importing)
                               &aux columns (cl-interpol:*list-delimiter* ", "))
  "Will make a best effor to create a table matching the csv's schema and then

   data-munger : a function that changes the the data row to be inserted
                 (for conversion to other types etc)
   progress-stream: the default log stream to print to
   progress-mod: how often we should report progress (in number of rows)
   log: is a function that accepts msg and args to display status updates to the user
   on-error: is a restart to run when an error is experienced, if nil, it will simply
            allow the error to go up the stack.  the default is to continue with the
            import, skipping the row that caused errors
  "
  (unless (eql column-names :first-row)
    (setf columns (data-table::sql-escaped-column-names
                   column-names
                   :transform column-transform)))
  (iter
    (for row in-csv file)
    (for cnt from 0)
    (if (and (eql column-names :first-row) (first-iteration-p))
        (setf columns (data-table::sql-escaped-column-names row :transform column-transform))
        (restart-case
            (handler-bind
                ((error (lambda (c)
                          (funcall log "Error importing ROW ~D of file: ~S~%~S~%~A~%~S"
                                   cnt file row c c)
                          (when on-error
                            (when (find-restart on-error) (invoke-restart on-error))))))
              (exec #?"INSERT INTO ${schema}.${table-name} (@{ columns })
VALUES ( @{ (funcall data-munger row) } )")
              (when (zerop (mod cnt progress-mod))
                (funcall log "Imported ~D rows~%" cnt )))
          (continue-importing ()
            :report "Continue Importing the file, skipping this row of data")))))
