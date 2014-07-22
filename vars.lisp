(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)

(defvar *quote* #\" "Default quote character")
(defvar *separator* #\, "Default separator character")
(defvar *escape-mode* :quote
  "Controls how escapes are handled.
    :quote - replace the entire *quote-escape* sequence with the quote
             character whenever we find it. Commonly used with \"\" quote
             escapes

    :following - replace the escape character and the following character with
             just the following character.
             EG: (*quote-escape* #\\ )
                 \\ -> \
                 \r -> r
                 \' -> '
   ")

;; we want to read basically anything by default
(defvar *read-newline* #?"\n"
        "Default newline string for reading.
         We trim extra whitespace by default *trim-outer-whitespace*")

;; we want to write toward excel by default
(defvar *write-newline* #?"\r\n"
        "When writing what should the newline convention be ")

(defvar *always-quote* nil "Default setting for always quoting")
(defvar *quote-escape* #?"${ *quote* }${ *quote* }" "Default setting for escaping quotes")
(defvar *unquoted-empty-string-is-nil* nil
  "Should unquoted empty string values, be nil or \"\".")

(defvar *quoted-empty-string-is-nil* nil
  "Should empty string values, be nil or \"\".
   Unquoted values are always trimmed of surrounding whitespace.
   Quoted values are never be trimmed")

(defvar *trim-outer-whitespace* t
  "Should white space between delimiters and data or quotes be removed

   These underscores (if they were spaces) are the locations in question
   'a',_b_,_' c '_,_d ")

(defvar *enable-signals* nil
  "Should the reading and writing process enable filtering signals")

(defvar *eof-char* #\null "The char we use for eof")

(defvar *buffer-size* 512)
