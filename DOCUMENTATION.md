	CL-CSV - a Common Lisp library for csv reading and writing
=====

# Abstract

A common lisp library providing easy csv reading and writing.

The code license is found
[here](https://github.com/AccelerationNet/cl-csv/blob/master/LICENSE).


# Contents

1.  [Download](#download)
2.  [The CL-CSV dictionary](#dictionary)
   1.  [`*default-external-format*`](#*default-external-format*)
   2.  [`*newline*`](#*newline*)
   3.  [`*quote*`](#*quote*)
   4.  [`*quote-escape*`](#*quote-escape*)
   5.  [`*separator*`](#*separator*)
   6.  [`csv-parse-error`](#csv-parse-error)
   7.  [`csv-parse-error`](#csv-parse-error)
   8.  [`do-csv`](#do-csv)
   9.  [`format-csv-value`](#format-csv-value)
   10. [`read-csv`](#read-csv)
   11. [`read-csv-row`](#read-csv-row)
   12. [`write-csv`](#write-csv)
   13. [`write-csv-row`](#write-csv-row)
   14. [`write-csv-value`](#write-csv-value)

3.  [Acknowledgements](#ack)



# Download

CL-CSV together with this documentation can be downloaded from
[https://github.com/AccelerationNet/cl-csv](https://github.com/AccelerationNet/cl-csv).

# The CL-CSV dictionary

_[Special variable]_
**\*default-external-format***

The external format used for opening files

_[Special variable]_
**\*newline***

 Default newline string

_[Special variable]_
**\*quote***

 Default quote character

_[Special variable]_
**\*quote-escape***

 Default setting for escaping quotes

_[Special variable]_
**\*separator***

 Default separator character

- - -
_[Condition type]_
**csv-parse-error**
- - -
_[Function]_
**csv-parse-error** ( *msg `&rest` args* ) =\> \*result*
- - -
_[Macro]_
**do-csv** *((row-var stream-or-pathname `&rest` read-csv-keys)
`&body` body)* =\> \*result*


row-var: a variable that is passed into _body_

stream-or-pathname: a stream or a pathname to read the CSV data from

read-csv-keys: keys and values passed to the _read-csv_ function

body: body of the macro

- - -
_[Generic function]_
**format-csv-value** ( *val* ) =\> \*result*

- - -
_[Method]_
**format-csv-value** ( *val* ) =\> \*result*

 Print values in ways that are most cross compatible with the csv
format
- - -
_[Function]_
**read-csv** ( *stream-or-string* `&key` *row-fn* *map-fn* *sample*
*skip-first-p* ( separator \*separator\* ) (quote \*quote\*) (escape
\*quote-escape\*))* =\> \*result*

Read in a CSV by data-row (which due to quoted newlines may be more
than one line from the stream)

row-fn: passing this parameter will cause this read to be streaming
and results will be discarded after the row-fn is called with data

map-fn: used for manipulating the data by row during collection if
specified; (funcall map-fn data) is collected instead of data

sample: when a positive integer, only take that many samples from
the input file

skip-first-p: when true, skips the first line in the csv

_Keywords:_

separator: character separating between data cells. Defaults to
\*separator*

quote: quoting character for text strings. Defaults to \*quote*

escape: escape character. Defaults to \*quote-escape*

- - -
_[Function]_
**read-csv-row** ( *stream-or-string* `&key` (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) `&aux` *current state line
llen c elen*) =\> \*result\*


Read in a CSV by _data-row_ (which due to quoted newlines may be more
than one line from the stream)

- - -
_[Function]_
**write-csv** (*rows-of-items* `&key` *stream* (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) (newline \*newline\*)
(always-quote \*always-quote\*) ) =\> \*result\*

Writes a CSV

rows-of-items: iterable

_Keywords:_

stream: stream to write to. Default: nil.

quote: quoting character. Defaults to \*quote\*

escape: escaping character. Defaults to \*quote-escape\*

newline: newline character. Defaults to \*newline\*

always-quote: Defaults to \*always-quote\*

- - -
_[Function]_
**write-csv-row** ( *items* `&key` *stream* (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) (newline \*newline\*)
(always-quote \*always-quote\*) ) =\> \*result*


Writes a list items to stream

rows-of-items: iterable

_Keywords:_

stream: stream to write to. Default: nil.

quote: quoting character. Defaults to \*quote\*

escape: escaping character. Defaults to \*quote-escape\*

newline: newline character. Defaults to \*newline\*

always-quote: Defaults to \*always-quote\*

- - -
_[Generic function]_
**write-csv-value** ( *val csv-stream* `&key` *formatter quote separator
escape always-quote* ) =\> \*result*

- - -
_[Method]_
**write-csv-value** ( *val csv-stream* `&key` *formatter quote separator
escape always-quote* ) =\> \*result*


Writes val to csv-stream in a formatted fashion.

_Keywords:_

formatter: used to format val. Defaults to format-csv-value.

quote: quoting character. Defaults to \*quote*

escape: escaping character. Defaults to \*quote-escape*

newline: newline character. Defaults to \*newline*

always-quote: Defaults to \*always-quote*

# Acknowledgements

This documentation was prepared with
[DOCUMENTATION-TEMPLATE](http://weitz.de/documentation-template/),
then passed through
[pandoc](http://johnmacfarlane.net/pandoc/index.html), *then* hand-edited.
