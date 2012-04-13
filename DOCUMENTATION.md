	CL-CSV - a Common Lisp library for csv reading and writing
----------------------------------------------------------

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



#Download

CL-CSV together with this documentation can be downloaded from
[https://github.com/AccelerationNet/cl-csv](https://github.com/AccelerationNet/cl-csv).

#The CL-CSV dictionary

[Special variable]
**\*default-external-format\***

>  the external format used for opening files

[Special variable]
**\*newline\***

>  Default newline string

[Special variable]
**\*quote\***

>  Default quote character

[Special variable]
**\*quote-escape\***

>  Default setting for escaping quotes

[Special variable]
**\*separator\***

>  Default separator character


[Condition type]
**csv-parse-error**

[Function]
**csv-parse-error** *msg `&rest` args* =\> *result*

[Macro]\
**do-csv** *(row-var stream-or-pathname `&rest` read-csv-keys)
declaration\* statement\** =\> *result*

>
> row-var: a variable that is passed into \_body\_
>
> stream-or-pathname: a stream or a pathname to read the CSV data from
>
> read-csv-keys: keys and values passed to the \_read-csv\_ function
>
> body: body of the macro

[Generic function]
**format-csv-value** *val* =\> *result*

[Method]
**format-csv-value** *val* =\> *result*

>  Print values in ways that are most cross compatible with the csv
> format

[Function]
**read-csv** *stream-or-string `&key` row-fn map-fn sample
skip-first-p (separator \*separator\*) (quote \*quote\*) (escape
\*quote-escape\*)* =\> *result*

> Read in a CSV by data-row (which due to quoted newlines may be more
> than one line from the stream)
>
> row-fn: passing this parameter will cause this read to be streaming
> and results will be discarded after the row-fn is called with data
>
> map-fn: used for manipulating the data by row during collection if
> specified; (funcall map-fn data) is collected instead of data
>
> sample: when a positive integer, only take that many samples from
> the input file
>
> skip-first-p: when true, skips the first line in the csv
>
> Keywords:
>
> separator: character separating between data cells. Defaults to
> \*separator\*
>
> quote: quoting character for text strings. Defaults to \*quote\*
>
> escape: escape character. Defaults to \*quote-escape\*


[Function]
**read-csv-row** *stream-or-string `&key` (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) `&aux` current state line
llen c elen* =\> *result*


>  Read in a CSV by data-row (which due to quoted newlines may be more
> than one line from the stream)


[Function]
**write-csv** *rows-of-items `&key` stream (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) (newline \*newline\*)
(always-quote \*always-quote\*)* =\> *result*

> Writes a CSV
>
> rows-of-items: iterable
>
> Keywords:
>
> stream: stream to write to. Default: nil.
>
> quote: quoting character. Defaults to \*quote\*
>
> escape: escaping character. Defaults to \*quote-escape\*
>
> newline: newline character. Defaults to \*newline\*
>
> always-quote: Defaults to \*always-quote\*

[Function]
**write-csv-row** *items `&key` stream (separator \*separator\*)
(quote \*quote\*) (escape \*quote-escape\*) (newline \*newline\*)
(always-quote \*always-quote\*)* =\> *result*


> Writes a list items to stream
>
> rows-of-items: iterable
>
> Keywords:
>
> stream: stream to write to. Default: nil.
>
> quote: quoting character. Defaults to \*quote\*
>
> escape: escaping character. Defaults to \*quote-escape\*
>
> newline: newline character. Defaults to \*newline\*
>
> always-quote: Defaults to \*always-quote\*

[Generic function]
**write-csv-value** *val csv-stream `&key` formatter quote separator
escape always-quote* =\> *result*

[Method]
**write-csv-value** *val csv-stream `&key` formatter quote separator
escape always-quote* =\> *result*


> Writes val to csv-stream in a formatted fashion.
>
> Keywords:
>
> formatter: used to format val. Defaults to format-csv-value.
>
> quote: quoting character. Defaults to \*quote\*
>
> escape: escaping character. Defaults to \*quote-escape\*
>
> newline: newline character. Defaults to \*newline\*
>
> always-quote: Defaults to \*always-quote\*

### Acknowledgements

This documentation was prepared with
[DOCUMENTATION-TEMPLATE](http://weitz.de/documentation-template/),
then passed through
[pandoc](http://johnmacfarlane.net/pandoc/index.html), *then* hand-edited.
