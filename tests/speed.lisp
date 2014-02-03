(defpackage :cl-csv-test.speed-tests
  (:use :cl :cl-user :cl-csv :lisp-unit2 :iter))

(in-package :cl-csv-test.speed-tests)

(defun run-speed-tests ()
  (lisp-unit2:run-tests
   :package :cl-csv-test.speed-tests
   :name :cl-csv-speed-tests
   :run-contexts #'lisp-unit2:with-summary-context))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(define-test write-big-file ()
  (let ((n 120000))
    (time-and-log-around (test-log "write large file test")
      (with-open-file (s +test-big-file+ :direction :output :if-exists :supersede )
        (iter (for i from 0 to n)
          (write-csv-row '("Russ" "Tyndall" "Software Developer's, \"Position\""
                           "26.2" "1" "further columns" "even" "more" "data")
                         :stream s))))))

(define-test count-big-file-csv-rows ()
  (let ((cnt 0))
    (time-and-log-around (test-log "read large file test")
      (read-csv +test-big-file+
                :row-fn (lambda (r) (declare (ignore r))
                          (incf cnt))
                ))
    cnt))

(define-test read-by-line-and-buffer (:tags '(cl-csv-test::read-until))
  (let ((cnt 0) (cnt2 0) (cnt3 0))
    (time-and-log-around (test-log "read large file by lines")
      (let ( line)
        (cl-csv::with-csv-input-stream (s +test-big-file+ )
          (handler-case
              (loop while (setf line (read-line s))
                    do (incf cnt))
            (end-of-file (c) (declare (ignore c)))))))

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

    (time-and-log-around (test-log "read large file by read-into-buffer-until")
      (cl-csv::with-csv-input-stream (s +test-big-file+ )
        (let ((buffer (make-string cl-csv::*buffer-size*)))
          (handler-case
              (loop
                while (plusp (cl-csv::read-into-buffer-until buffer s))
                do (incf cnt3))
            (end-of-file (c) (declare (ignore c)))))))

    (format lisp-unit2:*test-stream*
            "~@:_ lines:~D , buffers:~D, buffered-lines:~D~@:_"
            cnt cnt2 cnt3)))

(define-test collect-big-file-csv-rows ()
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

