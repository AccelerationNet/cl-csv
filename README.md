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

 * http://members.optusnet.com.au/apicard/csv-parser.lisp
 * http://www.cliki.net/fare-csv
 * 

## Library Integration

 * data-table - functions for building data-tables from csv's
 * clsql
   * import-from-csv
   * serial-import-from-csv


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