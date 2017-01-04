(defpackage :cl-csv-test
  (:use :cl :cl-user :cl-csv :lisp-unit2 :iter))

(in-package :cl-csv-test)
(cl-interpol:enable-interpol-syntax)

(defun run-all-tests ()
  (run-tests
   :package :cl-csv-test
   :name :cl-csv
   :run-contexts #'with-summary-context ))

(defmacro assert-length (exp it &rest them)
  `(assert-eql ,exp (length ,it) ,@them))

(defparameter +test-csv-quoted-path+
  (asdf:system-relative-pathname :cl-csv "tests/test-csv-quoted.csv"))
(defparameter +test-csv-unquoted-path+
  (asdf:system-relative-pathname :cl-csv "tests/test-csv-unquoted.csv"))
(defparameter +test-csv-unquoted-no-trailing-path+
  (asdf:system-relative-pathname :cl-csv "tests/test-csv-unquoted-no-trailing.csv"))
(defparameter +test-multiline+
  (asdf:system-relative-pathname :cl-csv "tests/test-multiline-data.csv"))
(defparameter +test-backslash-escapes+
  (asdf:system-relative-pathname :cl-csv "tests/test-backslash-escapes.csv"))

(defparameter +test-files+
  (list
   +test-csv-quoted-path+
   +test-csv-unquoted-path+
   +test-csv-unquoted-no-trailing-path+))

(defparameter *test-csv1-rows*
  '(("first name" "last name" "job \"title\"" "number of hours" "id")
    ("Russ" "Tyndall" "Software Developer's, \"Position\"" "26.2" "1")
    ("Adam" "Smith" "Economist" "37.5" "2")
    ("John" "Doe" "Anonymous Human" "42.1" "3")
    ("Chuck" "Darwin" "Natural Philosipher" "17.68" "4")
    ("Bill" "Shakespear" "Bard" "12.2" "5")
    ("James" "Kirk" "Starship Captain" "13.1" "6")
    ("Bob" "Anon" "" "13.1" "6")
    ("Mr" "Iñtërnâtiônàlizætiøn" "" "1.1" "0")))

(defparameter *test-csv1*
"\"first name\",\"last name\",\"job \"\"title\"\"\",\"number of hours\",\"id\"
\"Russ\",\"Tyndall\",\"Software Developer's, \"\"Position\"\"\",\"26.2\",\"1\"
\"Adam\",\"Smith\",\"Economist\",\"37.5\",\"2\"
\"John\",\"Doe\",\"Anonymous Human\",\"42.1\",\"3\"
\"Chuck\",\"Darwin\",\"Natural Philosipher\",\"17.68\",\"4\"
\"Bill\",\"Shakespear\",\"Bard\",\"12.2\",\"5\"
\"James\",\"Kirk\",\"Starship Captain\",\"13.1\",\"6\"
\"Bob\",\"Anon\",\"\",\"13.1\",\"6\"
\"Mr\",\"Iñtërnâtiônàlizætiøn\",\"\",\"1.1\",\"0\"
")

(defparameter *test-csv1-v2*
"first name,last name,\"job \"\"title\"\"\",number of hours,id
Russ,Tyndall,\"Software Developer's, \"\"Position\"\"\",26.2,1
Adam,Smith,Economist,37.5,2
John,Doe,Anonymous Human,42.1,3
Chuck,Darwin,Natural Philosipher,17.68,4
Bill,Shakespear,Bard,12.2,5
James,Kirk,Starship Captain,13.1,6
Bob,Anon,,13.1,6
Mr,Iñtërnâtiônàlizætiøn,,1.1,0
")

(defparameter *test-csv-no-trailing-newline*
  "first name,last name,\"job \"\"title\"\"\",number of hours,id
Russ,Tyndall,\"Software Developer's, \"\"Position\"\"\",26.2,1")

(defparameter *test-csv-data-with-newlines*
  "first name,last name,\"job \"\"title\"\"\",number of hours,id
Russ,Tyndall,\"Software Developer's,
 \"\"Position\"\"\",26.2,1")

(defparameter *test-csv-data-waiting-next-error*
  "\"Which of the following is an appropriate calming technique or statement:
A. \"\"I can help you.\"\"
B. \"\"Shut up.\"\"
C. \"\"If you don't calm down I'm not sending anyone.\"\"
D. \"\"Ma'am, ma'am\ ma'am!\"\"\",A")

(define-test parsing-1 (:tags '(parsing))
  (assert-equal *test-csv1-rows* (read-csv *test-csv1*))
  (assert-equal *test-csv1-rows* (read-csv *test-csv1-v2*)))

(define-test writing-1 (:tags '(writing))
  (assert-equal *test-csv1* (write-csv *test-csv1-rows* :always-quote t)))

(define-test parsing-errors (:tags '(parsing errors))
  (assert-error 'csv-parse-error
      (read-csv-row
       "first name, a test\" broken quote, other stuff"))
  (assert-error 'csv-parse-error
      (read-csv-row
       "first name,\"a test broken quote\" what are these chars, other stuff"))
  (assert-error 'csv-parse-error
      (read-csv-row
       "first name,\"a test unfinished quote, other stuff"))
  (assert-eql 3 (length (read-csv-row "first name, \"a test broken quote\", other stuff")))
  )

(define-test no-trailing-parse (:tags '(parsing errors))
  (let* ((data (read-csv *test-csv-no-trailing-newline*))
         (str (write-csv data :always-quote t))
         (data2 (read-csv str)))
    (assert-equal 2 (length data))
    (assert-equal 5 (length (first data)))
    (assert-equal 5 (length (second data)))
    (assert-equal data data2)))

(define-test data-with-newlines (:tags '(whitespace parsing writing))
  (let* ((data (read-csv *test-csv-data-with-newlines*))
         (str (write-csv data :always-quote t))
         (data2 (read-csv str)))
    (assert-equal 2 (length data))
    (assert-equal 5 (length (first data)))
    (assert-equal 5 (length (second data)))
    (assert-equal
        "Software Developer's,
 \"Position\""
        (third (second data)))
    (assert-equal data data2)))

(define-test data-with-whitespace-trim (:tags '(whitespace parsing trim))
  (assert-equal
   '("first" "last" " other " "" nil nil)
   (read-csv-row "  first    ,     last ,  ' other ','',,  "
                 :unquoted-empty-string-is-nil t
                 :quoted-empty-string-is-nil nil
                 :trim-outer-whitespace t
                 :quote #\'))
  (assert-equal
   '("  first    " "     last " " other " "" nil " ")
   (read-csv-row "  first    ,     last ,' other ','',, "
                 :unquoted-empty-string-is-nil t
                 :quoted-empty-string-is-nil nil
                 :trim-outer-whitespace nil
                 :quote #\'))

  (assert-error 'csv-parse-error
   (read-csv-row "  first    ,     last , ' other ','',, "
                 :unquoted-empty-string-is-nil t
                 :quoted-empty-string-is-nil nil
                 :trim-outer-whitespace nil
                 :quote #\')
   "whitespace  before quoted values is a parse error if we are
    not trimming ")
  (assert-error 'csv-parse-error
   (read-csv-row "  first    ,     last ,' other ' ,'',, "
                 :unquoted-empty-string-is-nil t
                 :quoted-empty-string-is-nil nil
                 :trim-outer-whitespace nil
                 :quote #\')
   "whitespace after quoted values is a parse error if we are
    not trimming ")
  )

(define-test data-with-whitespace-nilling (:tags '(whitespace parsing trim))
  (assert-equal
   '("first" "last" " other " nil nil nil)
   (read-csv-row "  first    ,     last ,  ' other '   ,'',,  "
                 :quoted-empty-string-is-nil t
                 :unquoted-empty-string-is-nil t
                 :quote #\'))
  (assert-equal
   '("first" "last" " other " "" "" "")
   (read-csv-row "  first    ,     last ,' other ','',, "
                 :quoted-empty-string-is-nil nil
                 :unquoted-empty-string-is-nil nil
                 :quote #\'))

  (assert-equal
   '("first" "last" " other " nil "" "")
   (read-csv-row "  first    ,     last , ' other ','',, "
                 :quoted-empty-string-is-nil T
                 :unquoted-empty-string-is-nil nil
                 :quote #\')
   "whitespace  before quoted values is a parse error if we are
    not trimming ")
  (assert-equal
   '("first" "last" " other " "" nil nil)
   (read-csv-row "  first    ,     last ,' other ' ,'',, "
                 :quoted-empty-string-is-nil nil
                 :unquoted-empty-string-is-nil t
                 :quote #\')
   "whitespace after quoted values is a parse error if we are
    not trimming ")
  )


(define-test files (:tags '(parsing files))
  (iter (for csv in +test-files+)
    (for data = (read-csv csv))
    (assert-equal *test-csv1-rows* data csv)))

(define-test multi-line-file (:tags '(parsing files))
  (let ((data (read-csv +test-multiline+)))
    (assert-equal 2 (length data) data)
    (assert-equal "test
of
multiline" (nth 3 (first data)) ))
  )

(define-test dont-always-quote-and-newline (:tags '(writing whitespace quotation))
  (let* ((row '("Russ" "Tyndall" "Software Developer's, \"Position\"" "26.2" "1" ","))
         (res (write-csv-row row :always-quote nil :newline #?"\n")))
    (assert-equal #?"Russ,Tyndall,\"Software Developer's, \"\"Position\"\"\",26.2,1,\",\"\n"
        res)))

(define-test dont-always-quote-and-newline-2 (:tags '(writing whitespace quotation))
  (let* ((row '("," #?"a\r\nnewline\r\ntest\r\n"))
         (res (write-csv-row row :always-quote nil :newline #?"\n")))
    (assert-equal #?"\",\",\"a\r\nnewline\r\ntest\r\n\"\n"
        res)))

(define-test cause-error (:tags '(parsing errors))
  (let ((data (read-csv *test-csv-data-waiting-next-error*)))
    (assert-true data)))

(define-test chars-in-test (:tags '(utils parsing))
  (assert-true (cl-csv::chars-in "a" "abcdef"))
  (assert-false (cl-csv::chars-in "qu" "abcdef"))
  (assert-true (cl-csv::chars-in "qu" "asdfqasdf"))
  (assert-true (cl-csv::chars-in "qu" "asdfuasdf"))
  (assert-true (cl-csv::chars-in (list "q" "u") "asdfuasdf"))
  (assert-true (cl-csv::chars-in (list #\q #\u) "asdfuasdf"))
  (assert-true (cl-csv::chars-in (list "q" #\u) "asdfqasdf")))

(define-test iterate-clauses (:tags '(utils iterate))
  (iter
    (for (a b c) in-csv "1,2,3
4,5,6")
    (assert-equal (if (first-time-p) "1" "4") a)
    (assert-equal (if (first-time-p) "2" "5") b)
    (assert-equal (if (first-time-p) "3" "6") c)
    (for i from 0)
    (finally (assert-equal 1 i)))

  ;; test SKIPPING-HEADER option
  (iter
    (for (a b c) in-csv "1,2,3
4,5,6" SKIPPING-HEADER T)
    (assert-equal  "4" a)
    (assert-equal  "5" b)
    (assert-equal  "6" c)
    (for i from 0)
    (finally (assert-equal 0 i)))

  ;; test SEPARATOR
  (iter
    (for (a b c) in-csv "1|2|3
4|5|6" SKIPPING-HEADER T SEPARATOR #\|)
    (assert-equal  "4" a)
    (assert-equal  "5" b)
    (assert-equal  "6" c)
    (for i from 0)
    (finally (assert-equal 0 i))))

(define-test sampling-iterate (:tags '(parsing iterate))
  (assert-length
   9 (iter (for row in-csv *test-csv1*)
       (cl-csv:sampling row)))
  (assert-length
   2 (iter (for row in-csv *test-csv1*)
       (cl-csv:sampling row into sample size 2)
       (finally (return sample))))
  (assert-length
   2 (read-csv-sample *test-csv1* 2))
  (assert-length
   3 (iter (for row in-csv *test-csv1* skipping-header t)
       (cl-csv::sampling row size 3)))
  (assert-length
   9 (iter (for row in-csv *test-csv1*)
       (cl-csv:sampling row into sample size 25)
       (finally (return sample)))))

(define-test csv-signal-enabling (:tags '(signals))
  (assert-signal
   'csv-row-read
   (assert-signal
    'csv-data-read
    (let ((*enable-signals* t))
      (cl-csv:read-csv "1,2,3"))))
  (assert-no-signal
   'csv-row-read
   (assert-no-signal
    'csv-data-read
    (let ((*enable-signals* nil))
      (cl-csv:read-csv "1,2,3")))))

(define-test csv-filter (:tags '(signals))
  (assert-equal
   '(1 2 3)
   (let ((*enable-signals* t))
      (handler-bind ((csv-data-read
                       (lambda (c) (invoke-restart 'filter (parse-integer (cl-csv::data c))))))
        (cl-csv:read-csv-row "1,2,3"))))
  (assert-equal
   '(1 2 3)
   (let ((*enable-signals* t))
      (handler-bind ((csv-row-read
                       (lambda (c) (invoke-restart 'filter (mapcar #'parse-integer (cl-csv::row c))))))
        (cl-csv:read-csv-row "1,2,3")))))

(defun displaced-sub-string (s &key (start 0) (end (length s)))
  (make-array (- end start)
              :element-type (array-element-type s)
              :displaced-to s
              :displaced-index-offset start))

(define-test csv-continue-signals (:tags '(signals))
  (handler-bind ((csv-parse-error #'continue))
    (assert-equal
     '(("1" "2" "3")
       ("3" "4" "5"))
     (cl-csv:read-csv "1,2,3
2,3',4
3,4,5" :quote #\'))))

(define-test early-end-of-stream (:tags '(errors parsing))
  (let ((line #?|"1","2|))
    (assert-error
     'cl-csv:csv-parse-error
     (cl-csv:read-csv-row line)))
  (let ((line ""))
    (assert-error
     'end-of-file
     (cl-csv:read-csv-row line)))

  (let ((line #?|"1","2|))
    (assert-equal
     '("1" "2")
     (handler-bind ((cl-csv:csv-parse-error
                      (lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'cl-csv::finish-item))))
       (cl-csv:read-csv-row line))))
  )

(define-test read-into-buffer-until-test (:tags '(read-until))
  ;; \r\l newline
  (with-input-from-string (in #?"test this\r\n thing")
    (let* ((s (make-string 80))
           (l (cl-csv::read-into-buffer-until s in :nl #?"\r\n")))
      (assert-eql 11 l)
      (assert-equal #?"test this\r\n" (displaced-sub-string s :end l))))
  ;; newline
  (with-input-from-string (in #?"t\nest this\n thing")
    (let* ((s (make-string 80))
          (l (cl-csv::read-into-buffer-until s in :nl #\newline)))
      (assert-eql 2 l)
      (assert-equal #?"t\n" (displaced-sub-string s :end l))
      (let ((l (cl-csv::read-into-buffer-until s in :nl #\newline)))
        (assert-eql 9 l)
        (assert-equal #?"est this\n" (displaced-sub-string s :end l)))))
  ;; EOF
  (with-input-from-string (in #?"test this thing")
    (let* ((s (make-string 80))
          (l (cl-csv::read-into-buffer-until s in :nl #\newline)))
      (assert-eql (length "test this thing") l)
      (assert-equal "test this thing" (displaced-sub-string s :end l))
      ))
  ;; filled buffer
  (with-input-from-string (in #?"test this thing")
    (let* ((s (make-string 4))
          (l (cl-csv::read-into-buffer-until s in :nl #\newline)))
      (assert-eql 4 l)
      (assert-equal "test" (displaced-sub-string s :end l))
      (assert-eql 4 (cl-csv::read-into-buffer-until s in :nl #\newline))
      (assert-eql 4 (cl-csv::read-into-buffer-until s in :nl #\newline))
      (assert-eql 3 (cl-csv::read-into-buffer-until s in :nl #\newline))
      (assert-error 'end-of-file (cl-csv::read-into-buffer-until s in :nl #\newline))
      )))

(define-test buffer-spanning-new-lines
    (:tags '(read-until whitespace parsing))
  (with-input-from-string (in "testRNtest")
    (let* ((s (make-string 5))
           len)
      (setf len
            (cl-csv::read-into-buffer-until s in :nl "RN"))
      (assert-eql 5 len)
      (setf len
            (cl-csv::read-into-buffer-until
             s in :nl "RN"
             :nl-match 0))
      (assert-eql 1 len )
      (setf len
            (cl-csv::read-into-buffer-until s in :nl "RN"))
      (assert-eql 4 len))))

(define-test buffer-spanning-new-lines2
    (:tags '(read-until newlines whitespace parsing))
  ;; ** newline
  (with-input-from-string (in "test**tes**te**test")
    (let* ((s (make-string 5))
           len (nl-idx -1))
      (flet ((rebind ( &optional new-nl-idx)
               (when new-nl-idx
                 (setf nl-idx new-nl-idx))
               (multiple-value-setq
                   (len)
                 (cl-csv::read-into-buffer-until
                  s in :nl "**"
                  :nl-match nl-idx))))
        (rebind)
        (assert-eql 5 len :first s)
        (rebind 0)
        (assert-eql 1 len :second)
        (rebind -1)
        (assert-eql 5 len :third)
        (rebind)
        (assert-eql 4 len :third))
      )))

(define-test different-newlines (:tags '(read-until newlines whitespace parsing))
  (with-input-from-string (in "a|b|c|d**1|2|3|4")
    (let* ((cl-csv::*buffer-size* 8)
           (rows (cl-csv:read-csv in :newline "**" :separator "|")))
      (assert-equal 2 (length rows))
      (assert-equal '("a" "b" "c" "d") (first rows))
      (assert-equal '("1" "2" "3" "4") (second rows)))
    )
  (with-input-from-string (in "a|b|c|d**1|2|3|4")
    (let ((rows (cl-csv:read-csv in :newline "**" :separator "|")))
      (assert-equal 2 (length rows))
      (assert-equal '("a" "b" "c" "d") (first rows))
      (assert-equal '("1" "2" "3" "4") (second rows)))
    )
  (with-input-from-string (in "a|b|c|d*1|2|3|4")
    (let ((rows (cl-csv:read-csv in :newline #\* :separator #\|)))
      (assert-equal 2 (length rows))
      (assert-equal '("a" "b" "c" "d") (first rows))
      (assert-equal '("1" "2" "3" "4") (second rows)))
    )
  (with-input-from-string (in "a|b|c|d*1|2|3|4")
    (let ((rows (cl-csv:read-csv in :newline "*" :separator "|")))
      (assert-equal 2 (length rows))
      (assert-equal '("a" "b" "c" "d") (first rows))
      (assert-equal '("1" "2" "3" "4") (second rows)))
    ))

(define-test backslash-escapes (:tags '(backslash escapes parsing))
  (lisp-unit2:assert-error
   'csv-parse-error
   (cl-csv:read-csv +test-backslash-escapes+ :escape #?|\"|))
  (let ((results (cl-csv:read-csv
                  +test-backslash-escapes+
                  :escape #\\ :escape-mode :following )))
    (assert-equal 2 (length results))
    (assert-equal `("id","timestamp","date","comment","something") (first results))
    ))

(defparameter +test-csv-bug18-path+
  (asdf:system-relative-pathname :cl-csv "tests/bug18.csv"))

(define-test issue-18 (:tags '(parsing bugs whitespace empty-line))  
  (let* ((results (cl-csv:read-csv-row
                   +test-csv-bug18-path+ :separator #\, :escape "\"\""))
         (long-ml (nth 19 results)))
    (assert-true results)
    (assert-equal
     "COPY (select t1.fspace, t3.fname, fad_target_classid, t2.name, num_client, num_client_big_imp, coverage 
from 
abc

order by t1.fspace, t1.fad_target_classid) TO STDOUT DELIMITER ',' NULL 'null' CSV QUOTE '\"'"
     long-ml)
    (assert-equal
     '("2014-11-07 10:02:17.302 CST" "gdt_new" "gdt_insight_new" "18445"
       "10.136.165.93:39143" "545c1a19.480d" "1" "COPY"
       "2014-11-07 09:02:17 CST" "6/1303255" "1197337058" "ERROR" "57014"
       "canceling statement due to statement timeout" "" "" "" "" ""
       "COPY (select t1.fspace, t3.fname, fad_target_classid, t2.name, num_client, num_client_big_imp, coverage 
from 
abc

order by t1.fspace, t1.fad_target_classid) TO STDOUT DELIMITER ',' NULL 'null' CSV QUOTE '\"'"
       "" "ProcessInterrupts, postgres.c:3314" "psql")
     results)))



(defparameter +test-tab-csv-issue-10+ "id	inches	name
1	72\"	Russ Tyndall
2	67\"	Amy Bobanolis
")

(define-test issue-10-tab-csv (:tags '(parsing bugs whitespace empty-line))
  (let* ((csv (read-csv +test-tab-csv-issue-10+ :quote #\nul :separator #\tab))
         (row1 (second csv)))
    (assert-equal "72\"" (second row1) csv)))
