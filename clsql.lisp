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
        (clsql:query sql :flatp T)
      (write-csv-row cols :stream s)
      (write-csv rows :stream s))))

(defun import-from-csv (table-name &rest keys
                        &key file data-table (sample-size 1000)
                        schema (should-have-serial-id "id")
                        excluded-columns row-fn
                        (log-fn #'(lambda (&rest args) (declare (ignore args))))
                        (log-frequency 1000))
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
  (let* ((cl-interpol:*list-delimiter* ",")
         (*print-pretty* nil)
         (dt (or data-table (get-data-table-from-csv file t t sample-size)))
         (keys (copy-list keys)))
    (dolist (k '(:file :data-table :row-fn :sample-size :log-fn :log-frequency))
      (remf keys k))
    (funcall log-fn "CSV scanned for type information")
    (when (and should-have-serial-id
               (member should-have-serial-id (data-table:column-names dt) :test #'string-equal))
      (error #?"This table already has an id column name `${should-have-serial-id}` Column! Perhaps you wish to turn off should-have-serial-id or assign it a different name?"))
    (apply #'data-table:ensure-table-for-data-table dt table-name keys)
    (funcall log-fn "Created table")
    (let ((row-num 1)
          (importer (data-table::make-row-importer
                     dt table-name :schema schema :excluded-columns excluded-columns
                                   :row-fn row-fn))
          (start-time (get-universal-time)))
      (labels
          ((log-progress (&optional (msg "Processing row"))
            (let ((elapsed (- (get-universal-time) start-time)))
              (funcall log-fn "~a ~a. ~ds elapsed (~,2f rows/sec) "
                       msg row-num elapsed (/ row-num elapsed))))
           (row-fn (row)
             (when (zerop (mod row-num log-frequency)) (log-progress))
             (funcall importer row)
             (incf row-num)))
        (funcall log-fn "Starting import")
        (cl-csv:read-csv file :skip-first-p T :row-fn #'row-fn)
        (log-progress "Finished, total processed: ")))))

(defun serial-import-from-csv (table-name
                               &key file
                               (column-names :first-row)
                               (schema "public") (column-transform
                                                  #'data-table::english->postgres)
                               (progress-stream T) (progress-mod 5000)
                               (data-munger (lambda (row)
                                              (mapcar #'clsql-helper:format-value-for-database
                                                      row)))
                               (log (lambda (msg &rest args)
                                      (apply #'format progress-stream msg args)))
                               (on-error 'continue-importing)
                               &aux (cnt 0) columns
                               (cl-interpol:*list-delimiter* ", "))
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
  (labels ((handled-insert (sql log row)
             (restart-case
                 (handler-bind
                     ((error (lambda (c)
                               (funcall log "Error importing ROW ~D of file: ~S~%~S~%~A~%~S"
                                        cnt file row c c)
                               (when on-error
                                 (when (find-restart on-error) (invoke-restart on-error))))))
                   (exec sql))
               (continue-importing ()
                 :report "Continue Importing the file, skipping this row of data")))
           (row-fn (row &aux data sql)
             (incf cnt)
             (when (and (eql column-names :first-row)
                        (= cnt 1))
               (setf columns
                     (data-table::sql-escaped-column-names row :transform column-transform))
               (return-from row-fn))
             (setf data (funcall data-munger row)
                   sql #?"INSERT INTO ${schema}.${table-name} (@{ columns }) VALUES ( @{data} )")
             (handled-insert sql log row)
             (when (zerop (mod cnt progress-mod))
               (funcall log "Imported ~D rows~%" cnt ))))
    (read-csv file :row-fn #'row-fn)))