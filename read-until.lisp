(in-package :cl-csv)

(defun read-into-buffer-until (buffer stream
                               &key (nl #\newline) nl-match
                               &aux
                               (c #\null)
                               (nl-idx (or nl-match -1))
                               (nl-len (etypecase nl
                                         (string (length nl))
                                         (character 1)))
                               (nl-len-1 (- nl-len 1))
                               (buffer-len (length buffer)))

  "This reads into a buffer until either the buffer is full or the
   we have read the newline character(s).

   If we read the newline characters they will be the last character(s) in the
   buffer
  "
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type character c)
   (type (or simple-string character) nl)
   (type fixnum nl-len nl-len-1 nl-idx buffer-len)
   (type (simple-array character) buffer))
  (dotimes (i buffer-len)
    (setf c (read-char stream nil *eof-char*))
    ;; look for newlines
    (let ((new-idx (+ 1 nl-idx)))
      (declare (type fixnum new-idx))
      (if (char= (etypecase nl
                   (string (schar nl new-idx))
                   (character nl))
                 c)
          (setf nl-idx new-idx)
          (setf nl-idx -1)))
    (when (char= *eof-char* c)
      (if (zerop i)
          (error 'end-of-file :stream stream)
          ;; read some, then got an end of file
          (return-from read-into-buffer-until
            i)))
    (setf (schar buffer i) c)
    ;; got the nl
    (when (= nl-len-1 nl-idx)
      (return-from read-into-buffer-until
        (+ 1 i))))
  ;; got a full buffer
  (return-from read-into-buffer-until
    buffer-len))