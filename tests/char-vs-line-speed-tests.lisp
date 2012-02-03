(in-package :cl-csv-test)

(defun read-by-line-and-char ( &optional (n 3))
  (iter (for i from 0 below n)
    (time-and-log-around (test-log "read large file by lines")
      (read-csv +test-big-file+
                :read-fn #'cl-csv::read-csv-row
                :row-fn #'(lambda (row) (declare (ignore row)))))
  
    (time-and-log-around (test-log "read large file by char")
      (read-csv +test-big-file+
                :read-fn #'cl-csv::read-csv-row-by-char
                :row-fn #'(lambda (row) (declare (ignore row))))))
  
  (values))


#|
---- long lines
CL-CSV-TEST> (read-by-line-and-char)
09:38:27 BEGIN read large file by lines
09:38:30   END read large file by lines
Evaluation took:
  3.631 seconds of real time
   3.528221 seconds of total run time (3.480218 user, 0.048003 system)
   [ Run times consist of 0.084 seconds GC time, and 3.445 seconds non-GC time. ]
   97.16% CPU
   9,055,522,440 processor cycles
   422,464,752 bytes consed
   

09:38:30 BEGIN read large file by char
09:38:35   END read large file by char
Evaluation took:
  4.197 seconds of real time
   4.012251 seconds of total run time (3.960247 user, 0.052004 system)
   [ Run times consist of 0.032 seconds GC time, and 3.981 seconds non-GC time. ]
   95.59% CPU
   10,467,159,390 processor cycles
   220,210,272 bytes consed
   

09:38:35 BEGIN read large file by lines
09:38:38   END read large file by lines
Evaluation took:
  3.720 seconds of real time
   3.536220 seconds of total run time (3.492218 user, 0.044002 system)
   [ Run times consist of 0.068 seconds GC time, and 3.469 seconds non-GC time. ]
   95.05% CPU
   9,277,287,728 processor cycles
   422,466,000 bytes consed
   

09:38:38 BEGIN read large file by char
09:38:43   END read large file by char
Evaluation took:
  4.235 seconds of real time
   4.020252 seconds of total run time (3.996250 user, 0.024002 system)
   [ Run times consist of 0.028 seconds GC time, and 3.993 seconds non-GC time. ]
   94.92% CPU
   10,561,480,732 processor cycles
   220,242,960 bytes consed
   

09:38:43 BEGIN read large file by lines
09:38:46   END read large file by lines
Evaluation took:
  3.623 seconds of real time
   3.484218 seconds of total run time (3.456216 user, 0.028002 system)
   [ Run times consist of 0.068 seconds GC time, and 3.417 seconds non-GC time. ]
   96.16% CPU
   9,034,269,938 processor cycles
   422,460,112 bytes consed
   

09:38:46 BEGIN read large file by char
09:38:50   END read large file by char
Evaluation took:
  4.185 seconds of real time
   4.012251 seconds of total run time (3.964248 user, 0.048003 system)
   [ Run times consist of 0.032 seconds GC time, and 3.981 seconds non-GC time. ]
   95.87% CPU
   10,436,186,235 processor cycles
   220,208,272 bytes consed

-----
short lines, many of them

09:40:54 BEGIN read large file by lines
09:40:56   END read large file by lines
Evaluation took:
  1.737 seconds of real time
   1.680106 seconds of total run time (1.660104 user, 0.020002 system)
   [ Run times consist of 0.028 seconds GC time, and 1.653 seconds non-GC time. ]
   96.72% CPU
   4,330,707,855 processor cycles
   225,717,520 bytes consed
   

09:40:56 BEGIN read large file by char
09:40:58   END read large file by char
Evaluation took:
  2.037 seconds of real time
   1.920120 seconds of total run time (1.888118 user, 0.032002 system)
   [ Run times consist of 0.024 seconds GC time, and 1.897 seconds non-GC time. ]
   94.26% CPU
   5,078,684,130 processor cycles
   174,741,376 bytes consed
   

09:40:58 BEGIN read large file by lines
09:40:59   END read large file by lines
Evaluation took:
  1.727 seconds of real time
   1.684105 seconds of total run time (1.656104 user, 0.028001 system)
   [ Run times consist of 0.040 seconds GC time, and 1.645 seconds non-GC time. ]
   97.51% CPU
   4,306,710,780 processor cycles
   225,727,296 bytes consed
   

09:40:59 BEGIN read large file by char
09:41:01   END read large file by char
Evaluation took:
  1.970 seconds of real time
   1.908119 seconds of total run time (1.896118 user, 0.012001 system)
   [ Run times consist of 0.028 seconds GC time, and 1.881 seconds non-GC time. ]
   96.85% CPU
   4,913,388,067 processor cycles
   174,708,640 bytes consed
   

09:41:01 BEGIN read large file by lines
09:41:03   END read large file by lines
Evaluation took:
  1.871 seconds of real time
   1.756110 seconds of total run time (1.728108 user, 0.028002 system)
   [ Run times consist of 0.044 seconds GC time, and 1.713 seconds non-GC time. ]
   93.85% CPU
   4,666,126,665 processor cycles
   225,728,624 bytes consed
   

09:41:03 BEGIN read large file by char
09:41:05   END read large file by char
Evaluation took:
  2.008 seconds of real time
   1.940121 seconds of total run time (1.936121 user, 0.004000 system)
   [ Run times consist of 0.032 seconds GC time, and 1.909 seconds non-GC time. ]
   96.61% CPU
   5,007,219,727 processor cycles
   174,741,184 bytes consed

|#