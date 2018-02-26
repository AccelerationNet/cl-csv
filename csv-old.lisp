;; -*- lisp -*-
(in-package :cl-csv)
(cl-interpol:enable-interpol-syntax)


;;;; This was the old state machine in local variables parser
;;;; its proven to be slower than the newer clos variant on newer sbcls
;;;;
(defun read-csv-row-old
    (stream-or-string
     &key
     ((:separator *separator*) *separator*)
     ((:quote *quote*) *quote*)
     ((:escape *quote-escape*) *quote-escape*)
     ((:unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*)
      *unquoted-empty-string-is-nil*)
     ((:quoted-empty-string-is-nil *quoted-empty-string-is-nil*)
      *quoted-empty-string-is-nil*)
     ((:trim-outer-whitespace *trim-outer-whitespace*)
      *trim-outer-whitespace*)
     ((:newline *read-newline*) *read-newline*)
     ((:escape-mode *escape-mode*) *escape-mode*)
     &aux
     (*quote-escape* (or *quote-escape*
                         #?"${ *quote* }${ *quote* }"))
     (*separator* (etypecase *separator*
                    (string (if (= 1 (length *separator*))
                                (schar *separator* 0)
                                (error "Only single character *separator* are currently supported:~A"
                                       *separator*)))
                    (character *separator*)))
     (*read-newline* (etypecase *read-newline*
                       (string (if (= 1 (length *read-newline*))
                                   (schar *read-newline* 0)
                                   *read-newline*))
                       (character *read-newline*)))
     (current (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
     (state :waiting)
     (i -1)
     (c #\null)
     (elen (etypecase *quote-escape*
             (string (length *quote-escape*))
             (character 1)))
     items items-tail
     (nl-match -1)
     (nl-len (etypecase *read-newline*
               (string (length *read-newline*))
               (character 1)))
     (nl-len-1 (- nl-len 1))
     (use-read-line? (etypecase *read-newline*
                       (character (char= *read-newline* #\newline))
                       (string nil)))
     newline-matched?
     (line (unless use-read-line?
             (make-array *buffer-size* :element-type 'character )))
     (llen -1)
     read-line-got-a-newline?)
  "Read in a CSV by data-row (which due to quoted newlines may be more than one
                              line from the stream)
  "
  ;; giant state machine parser
  ;; states:
  ;;    waiting: we are between inputs, or have not started reading yet
  ;;    collecting: collecting unquoted data
  ;;    collecting-quoted: collecting quoted data
  ;;    waiting-on-next: done collecting quoted data, now waitin for a
  ;;        separator

  ;; this just ensures that a file opened here is closed here
  (with-csv-input-stream (in-stream stream-or-string)
    (csv-row-read
     (block results
       (loop
         do (labels ((set-loop-vars ()
                       (when (and line (< i llen))
                         (setf c (schar line i))
                         (let ((new-idx (+ 1 nl-match)))
                           (declare (type fixnum new-idx))
                           (if (char= (etypecase *read-newline*
                                        (string (schar *read-newline* new-idx))
                                        (character *read-newline*))
                                      c)
                               (setf nl-match new-idx)
                               (setf nl-match -1))
                           (setf newline-matched?
                                 (= nl-len-1 nl-match)))))

                     (current-last-char ()
                       (elt current (- (fill-pointer current) 1)))

                     (store-char (char)
                       (typecase char
                         (character (vector-push-extend char current))
                         (string (iter (for c in-string char)
                                   (vector-push-extend c current)))))

                     (finish-item ()
                       ;; trim off unquoted whitespace at the end
                       (when (and (eql state :collecting) *trim-outer-whitespace*)
                         (iter (while (white-space? (current-last-char)))
                           (decf (fill-pointer current))))

                       ;; collect the result
                       (let ((v (cons
                                 (if (and
                                      ;; got a zero length string?
                                      (zerop (length (string current)))
                                      ;; should we collect nil for zero length strings?
                                      (or (and (member state '(:waiting))
                                               *unquoted-empty-string-is-nil*)
                                          (and (member state '(:waiting-for-next))
                                               *quoted-empty-string-is-nil*)))
                                     (csv-data-read nil)
                                     (csv-data-read (copy-seq (string current))))
                                 nil)))
                         (if items
                             (setf (cdr items-tail) v
                                   items-tail v)
                             (setf items v
                                   items-tail v)))
                       ;; go back to waiting for items
                       (setf state :waiting)
                       (setf (fill-pointer current) 0))

                     (skip-escape ()
                       (dotimes (j (- elen 1))
                         (next-char)))

                     (read-line-in ()
                       (handler-bind
                           ((end-of-file
                              (lambda (sig)
                                (ecase state
                                  (:waiting
                                   ;; let the signal go through, we have not read anything and already EOF
                                   (when items
                                     (finish-item)
                                     (return-from results items))
                                   )
                                  (:waiting-for-next
                                   ;; finished reading before encountering the next separator
                                   (return-from results items))
                                  (:collecting
                                      ;; finished reading the file so must have finished this item
                                      (finish-item)
                                    (return-from results items))
                                  (:collecting-quoted
                                   (restart-case (csv-parse-error "End of file while collecting quoted item: ~A" sig)
                                     (finish-item ()
                                       (finish-item)
                                       (return-from results items))))))
                              ))
                         ;; reset index, line and len for the next line of data
                         (setf i 0) ;; we will increment immediately after this
                         (if use-read-line?
                             (multiple-value-bind (line-in didnt-get-a-newline?)
                                 (read-line in-stream)
                               (setf line line-in
                                     read-line-got-a-newline? (not didnt-get-a-newline?)
                                     llen (length line)))
                             (setf llen
                                   (read-into-buffer-until
                                    line in-stream
                                    :nl *read-newline*
                                    :nl-match nl-match)))
                         (set-loop-vars)))

                     (read-line-if-needed ()
                       (cond
                         ;; if we dont have a line yet read one
                         ((minusp llen) (read-line-in))

                         ;; we made it to the end of our buffer, so start again
                         ((>= i llen)
                          (ecase state
                            (:collecting-quoted
                             (when use-read-line?
                               (when read-line-got-a-newline?
                                 (store-char *read-newline*))))
                            ((:waiting :collecting :waiting-for-next)
                             (when newline-matched?
                               (decf (fill-pointer current) nl-len))
                             (when (or newline-matched? use-read-line?)
                               (finish-item)
                               (return-from results items))))
                          (read-line-in))))

                     (next-char ()
                       (incf i)
                       (read-line-if-needed)
                       (unless (zerop i)
                         (set-loop-vars)))

                     (handle-character ()
                       (cond
                         ;; read an empty line, next iteration
                         ((and
                           (member state '(:collecting :collecting-quoted))
                           (= i llen 0))
                          nil)

                         ;; the next characters are an escape sequence, start skipping
                         ((and (or (member state '(:collecting :collecting-quoted)))
                               *quote-escape* ;; if this is null there is no escape
                               (%escape-seq? line i *quote-escape* llen elen))
                          ;; this skips to the last char so that our next loop
                          ;; will start at the next char
                          (skip-escape)
                          ;; TODO: *escape-mode* needs to happen on writing too
                          (ecase *escape-mode*
                            (:quote (store-char *quote*))
                            (:following
                             ;; we need to immediately go to the next char and store
                             ;; without processing
                             (next-char)
                             (store-char c))))

                         ;; the character is data separator, so gather the word unless
                         ;; it is quoted data
                         ((char= *separator* c)
                          (ecase state
                            (:collecting-quoted (store-char c))
                            ((:collecting :waiting :waiting-for-next)
                             (finish-item))))

                         ;; the character is a quote (and not an escape) so start an item
                         ;; finishing the item is the responsibility of separator/eol
                         ((and *quote* (char= *quote* c))
                          (ecase state
                            (:waiting (setf state :collecting-quoted))
                            (:collecting-quoted
                             ;; if we end up trying to read
                             (setf state :waiting-for-next))
                            (:collecting
                                (csv-parse-error "we are reading non quoted csv data and found a quote at ~D~%~A"
                                                 i line))))

                         (t             ;; regular character
                          (ecase state
                            (:waiting
                             (unless (and *trim-outer-whitespace* (white-space? c))
                               (setf state :collecting)
                               (store-char c)))
                            (:waiting-for-next
                             (unless (and *trim-outer-whitespace* (white-space? c))
                               (csv-parse-error
                                "We finished reading a quoted value and got more characters before a separator or EOL ~D~%~A"
                                i line)))
                            ((:collecting :collecting-quoted)
                             (store-char c)))))))
              (next-char)
              (handle-character)))))))


(defun read-csv-old (stream-or-string
                 &key row-fn map-fn sample skip-first-p
                 ((:separator *separator*) *separator*)
                 ((:quote *quote*) *quote*)
                 ((:escape *quote-escape*) *quote-escape*)
                 ((:unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*)
                  *unquoted-empty-string-is-nil*)
                 ((:quoted-empty-string-is-nil *quoted-empty-string-is-nil*)
                  *quoted-empty-string-is-nil*)
                 ((:trim-outer-whitespace *trim-outer-whitespace*)
                  *trim-outer-whitespace*)
                 ((:newline *read-newline*) *read-newline*)
                 ((:escape-mode *escape-mode*) *escape-mode*))
  "Read in a CSV by data-row (which due to quoted newlines may be more than one
                              line from the stream)

row-fn: passing this parameter will cause this read to be streaming
           and results will be discarded after the row-fn is called
           with data

map-fn: used for manipulating the data by row during collection if
specified; (funcall map-fn data) is collected instead of data

sample: when a positive integer, only take that many samples from the input file

skip-first-p: when true, skips the first line in the csv


Keywords:

separator: character separating between data cells. Defaults to *separator*

quote: quoting character for text strings. Defaults to *quote*

escape: escape character. Defaults to *quote-escape*"

  (if sample
      (read-csv-sample stream-or-string sample
                       :row-fn row-fn :map-fn map-fn :skip-first-p skip-first-p)
      (iter
        (for data in-csv stream-or-string skipping-header skip-first-p)
        (if row-fn
            (funcall row-fn data)
            (collect (if map-fn (funcall map-fn data) data))))))
