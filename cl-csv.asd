;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-csv.system)
    (defpackage :cl-csv.system
	(:use :common-lisp :asdf))))

(in-package cl-csv.system)

(defsystem :cl-csv
  :author "Russ Tyndall (russ@acceleration.net), Acceleration.net"
  :description "Facilities for reading and writing CSV format files"
  :licence "BSD"
  :version "1.0"
  :serial t
  :components ((:file "packages")
               (:file "vars")
               (:file "read-until")
               (:file "csv"))
  :depends-on (:iterate :alexandria :cl-interpol))

(defsystem :cl-csv-test
  :author "Russ Tyndall (russ@acceleration.net), Acceleration.net"
  :description "Tests for a library providing a cl-csv class, and useful
     functionality around this"
  :licence "BSD"
  :version "1.0"
  :components ((:module :tests
			:serial t
			:components ((:file "csv"))))
  :depends-on (:cl-csv :lisp-unit2))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :cl-csv))))
  (asdf:oos 'asdf:load-op :cl-csv-test)
  (let ((*package* (find-package :cl-csv-test)))
    (eval (read-from-string "(run-all-tests)"))))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
