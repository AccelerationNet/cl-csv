(in-package :cl-csv-test)

(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun test-log (message &rest args)
  (format *standard-output* "~&")
  (log-time (get-universal-time) *standard-output*)
  (apply #'format *standard-output* message args)
  (format *standard-output* "~%"))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defmacro log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((gmessage (gensym "GMESSAGE-")))
      `(let ((,gmessage ,message))
	 (flet ((msg (&optional tag)
		  (format nil "~A ~a"
			  tag ,gmessage)))
	   (,log-name (msg "BEGIN") ,@args)
	   (multiple-value-prog1
	       (progn ,@body)
	     (,log-name (msg "  END") ,@args))))))

  (defmacro time-and-log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((trace-output (gensym "TRACE-OUTPUT-")))
      `(let (,trace-output) ;;leave nil so the first log call doesn't print an extra newline
	 (log-around (,log-name ,(concatenate 'string message "~@[~%~a~]") ,@args ,trace-output)
	   (setf ,trace-output
		 (make-array 10 :element-type 'character :adjustable T :fill-pointer 0))
	   (with-output-to-string (*trace-output* ,trace-output)
	     (time (progn ,@body))))))))

(defparameter +test-big-file+
  (asdf:system-relative-pathname :cl-csv "tests/long-test.csv"))

(defun ensure-big-file  (&optional (n 12000)
                           (m 25))
  (time-and-log-around (test-log "Ensure large file test")
    (with-open-file (s +test-big-file+ :direction :output :if-exists :supersede )
      (iter (for i from 0 below n)
        (write-csv-row
         (iter (for j from 0 below m)
           (appending '("Russ" "Tyndall" "Software Developer's, \"Position\""
                        "26.2" "1" "further columns" "even" "more" "data")))
         :stream s)))))

(defun count-big-file-csv-rows (&aux (cnt 0))
  (time-and-log-around (test-log "read large file test")
    (read-csv +test-big-file+
              :row-fn (lambda (r) (declare (ignore r))
                        (incf cnt))
              ))
  cnt)

(defun read-by-line-and-buffer (&aux (cnt 0) (cnt2 0))
  (time-and-log-around (test-log "read large file by lines")
    (cl-csv::with-csv-input-stream (s +test-big-file+ )
    (iter (for line in-stream s using #'read-line )
      (incf cnt))))
  
  (time-and-log-around (test-log "read large file by buffer")
    (cl-csv::with-csv-input-stream (s +test-big-file+ )
      (iter
        (with buffer = (make-array 80 :element-type 'character ))
        (with fill)
        (handler-case (setf fill (read-sequence buffer s))
          (end-of-file () (finish)))
        (incf cnt2)
        (while (= 80 fill))
        )))

  (values cnt cnt2))

(defun read-by-line-and-char ( &optional (n 3))
  (iter (for i from 0 below n)
    (time-and-log-around (test-log "read large file by buffer")
      (read-csv +test-big-file+
                :read-fn #'cl-csv::read-csv-row-by-buffer
                :row-fn #'(lambda (row) (declare (ignore row)))))
  
    (time-and-log-around (test-log "read large file by char")
      (read-csv +test-big-file+
                :read-fn #'cl-csv::read-csv-row-by-char
                :row-fn #'(lambda (row) (declare (ignore row)))))
    
    (time-and-log-around (test-log "read large file by lines")
      (read-csv +test-big-file+
                :read-fn #'cl-csv::read-csv-row
                :row-fn #'(lambda (row) (declare (ignore row))))))
  
  (values))


(defun collect-big-file-csv-rows ()
  (time-and-log-around (test-log "read large file test")
    (read-csv +test-big-file+))
  nil ; so we dont print 10m to the repl
  )

(defun process-csv2 (csv-name process-function )
  (let ( collector )
    (labels ((row-processor (row)
               (let ((result (funcall process-function row)))
                 (when result
                   (push result collector)))))
      (cl-csv:read-csv csv-name :row-fn #'row-processor)
      collector)))

(defun return-blanks2 (filename)
  (labels ((row-checker (row &aux (6th (sixth row)) )
             (if (string-equal 6th "")
                 (list (first row) (sixth row)))))
    (process-csv2 filename #'row-checker)))

(defun test-pnathan-code2 ()
  (time-and-log-around (test-log "test-pnathan2 read")
    (return-blanks2 +test-big-file+)))

;; (test-pnathan-code)
;; 15:57:18 BEGIN test-pnathan read
;; 15:57:21   END test-pnathan read
;; Evaluation took:
;;   3.480 seconds of real time
;;    3.390000 seconds of total run time (3.200000 user, 0.190000 system)
;;    [ Run times consist of 0.170 seconds GC time, and 3.220 seconds non-GC time. ]
;;    97.41% CPU
;;    8,678,145,157 processor cycles
;;    229,327,728 bytes consed

