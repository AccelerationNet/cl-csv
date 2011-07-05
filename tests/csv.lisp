(defpackage :cl-csv-test
  (:use :cl :cl-user :cl-csv :lisp-unit :iter))

(in-package :cl-csv-test)

(defparameter *test-csv1-rows*
  '(("first name" "last name" "job \"title\"" "number of hours" "id")
    ("Russ" "Tyndall" "Software Developer's, \"Position\"" "26.2" "1")
    ("Adam" "Smith" "Economist" "37.5" "2")
    ("John" "Doe" "Anonymous Human" "42.1" "3")
    ("Chuck" "Darwin" "Natural Philosipher" "17.68" "4")
    ("Bill" "Shakespear" "Bard" "12.2" "5")
    ("James" "Kirk" "Starship Captain" "13.1" "6")))

(defparameter *test-csv1*
"\"first name\",\"last name\",\"job \"\"title\"\"\",\"number of hours\",\"id\"
\"Russ\",\"Tyndall\",\"Software Developer's, \"\"Position\"\"\",\"26.2\",\"1\"
\"Adam\",\"Smith\",\"Economist\",\"37.5\",\"2\"
\"John\",\"Doe\",\"Anonymous Human\",\"42.1\",\"3\"
\"Chuck\",\"Darwin\",\"Natural Philosipher\",\"17.68\",\"4\"
\"Bill\",\"Shakespear\",\"Bard\",\"12.2\",\"5\"
\"James\",\"Kirk\",\"Starship Captain\",\"13.1\",\"6\"
")

(defparameter *test-csv1-v2*
"first name,last name,\"job \"\"title\"\"\",number of hours,id
Russ,Tyndall,\"Software Developer's, \"\"Position\"\"\",26.2,1
Adam,Smith,Economist,37.5,2
John,Doe,Anonymous Human,42.1,3
Chuck,Darwin,Natural Philosipher,17.68,4
Bill,Shakespear,Bard,12.2,5
James,Kirk,Starship Captain,13.1,6
")

(define-test verify-parsing-1
  (assert-equal *test-csv1-rows* (read-csv *test-csv1*))
  (assert-equal *test-csv1-rows* (read-csv *test-csv1-v2*)))

(define-test verify-writing-1
  (assert-equal *test-csv1* (write-csv *test-csv1-rows*)))

(define-test verify-parsing-errors
  (assert-error 'csv-parse-error
      (read-line-csv
       "first name, a test\" broken quote, other stuff"))
  (assert-error 'csv-parse-error
      (read-line-csv
       "first name,\"a test broken quote\" what are these chars, other stuff"))
  (assert-error 'csv-parse-error
      (read-line-csv
       "first name,\"a test unfinished quote, other stuff"))
  )