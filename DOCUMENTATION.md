
*Macro* **do-csv** 

**Syntax:**

**do-csv** ((row-var stream-or-pathname &rest read-csv-keys) &body body => *list-of-rows*


**Arguments and Values**

*row-var* -- a variable that is passed into _body_

*stream-or-pathname* -- a stream or a pathname to read the CSV data from

*read-csv-keys* -- keys and values passed to the _read-csv_ function

*body* -- body of the macro

*Function* **read-csv**

**Syntax:**

**read-csv** (stream-or-string
                 &key row-fn map-fn sample skip-first-p
                 ((:separator *separator*) *separator*)
                 ((:quote *quote*) *quote*)
                 ((:escape *quote-escape*) *quote-escape*))
				 
**Arguments and Values**

*stream-or-string* -- a stream or a pathname to read the CSV from

*row-fn* -- passing this parameter will cause this read to be streaming and results
           will be discarded after the row-fn is called with data

*map-fn* -- used for manipulating the data by row during collection if specified
           (funcall map-fn data) is collected instead of data

*sample* -- when a positive integer, only take that many samples from the input file

*skip-first-p* -- when true, skips the first line in the csv

*separator* -- separator between data cells

*quote* -- quote for text

*escape* -- escape character



*Function* csv-parse-error


*Function* format-csv-value
*Function* write-csv-value
*Function* write-csv-row
*Function* read-csv-row
*Function* write-csv
*Function* read-csv
*Function* export-query
*Function* import-from-csv 
*Function* serial-import-from-csv
*Function* get-data-table-from-csv


# Globals 
*default-external-format*
*quote*
*separator* 
*newline* 
*quote-escape*
