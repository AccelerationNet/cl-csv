# cl-csv

This library aims to simplify working with csvs to the bare minimum of tedium

* reads/writes csvs from/to strings, streams and files
* support streaming reads (allowing processing very large csvs, through read-csv's row-fn paramter)
* supports custom data formating
* settable quote, separator and quote-escapes
* supports multiline quoted data
* A test suite

## Rationale 

I had many scattered, not well tested, not easily runable pieces of
csv code.  I was unhappy with this situation then decided to refactor
all of this into a single location.  I wrote tests for it and had a
library so I thought I might release it.  This project started as
extensions and bugfixes on arnesi's CSV.

I then looked around and saw there are other csv libs out there that 
probably mostly accomplished what I had set out to do. However, I
already had code that was tested and had an easier license (BSD) so, I 
figured why not just release it anyway.

### Other Available CSV libs
 * http://members.optusnet.com.au/apicard/csv-parser.lisp
 * http://www.cliki.net/fare-csv

## Escaping and quotes

There are two modes for escaping currently
 * :quote - by default cl-csv treats `""` as an escape for a single double-quote
 * :following - read the character after the escape sequence verbatim, commonly 
   the `*quote-escape*` will be set to `#\\` when the escape mode is following.

## Signals and Restarts
 * `*enable-signals*` will cause a csv-data-read or csv-row-read to be
   signaled for each piece of data and row read.  There is a `filter`
   restart available which will cause the filter value to be used
   instead.  Enabling signals is ~2xs as slow as not, so by default
   they are not enabled.
 * `in-csv` iterate clause and `read-csv` support `continue` and `filter`
   restarts for errors occuring during read-csv-row


## Library Integration

 * [data-table](https://github.com/AccelerationNet/data-table) - functions for building data-tables from csv's, must `(asdf:load-system :cl-csv-data-table)`
 * [clsql](http://clsql.b9.com/) must `(asdf:load-system :cl-csv-clsql)`
    * import-from-csv
    * serial-import-from-csv
 * [iterate](http://common-lisp.net/project/iterate) - provides an
   `in-csv` driver clause for iterating over a CSV

## Examples

```lisp
;; read a file into a list of lists
(cl-csv:read-csv #P"file.csv")
=> (("1" "2" "3") ("4" "5" "6"))

;; read a file that's tab delimited
(cl-csv:read-csv #P"file.tab" :separator #\Tab)

;; read a file and return a list of objects created from each row
(cl-csv:read-csv #P"file.csv"
                 :map-fn #'(lambda (row)
                             (make-instance 'object
                                            :foo (nth 0 row)
                                            :baz (nth 2 row))))
;; read csv from a string (streams also supported)
(cl-csv:read-csv "1,2,3
4,5,6")
=> (("1" "2" "3") ("4" "5" "6"))

;; loop over a CSV for effect
(let ((sum 0))
  (cl-csv:do-csv (row #P"file.csv")
    (incf sum (parse-integer (nth 0 row))))
  sum)
  
  
;; loop over a CSV using iterate
(iter (for (foo bar baz) in-csv #P"file.csv")
  (collect (make-instance 'object :foo foo :baz baz)))
```

## Authors
 * [Acceleration.net](http://www.acceleration.net/)
    * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
    * [Nathan Bird](http://the.unwashedmeme.com/blog)
    * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
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
```
