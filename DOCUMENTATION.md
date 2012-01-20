
*Macro* **do-csv** 

**Syntax:**

**do-csv** ((row-var stream-or-pathname &rest read-csv-keys) &body body => *list-of-rows*


**Arguments and Values**

*row-var* -- a variable that is passed into _body_

*stream-or-pathname* -- a stream or a pathname to read the CSV data from

*read-csv-keys* -- keys for the _read-csv_ function

*body* -- body of the macro

*Function* **read-csv**

**Syntax:**

**read-csv** (stream-or-string
                 &key row-fn map-fn sample skip-first-p
                 ((:separator *separator*) *separator*)
                 ((:quote *quote*) *quote*)
                 ((:escape *quote-escape*) *quote-escape*))
				 
**Arguments and Values**

*stream-or-string*

*row-fn*

*map-fn*

*sample*

*skip-first-p*

*separator*

*quote*

*escape*



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
