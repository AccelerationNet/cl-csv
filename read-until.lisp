(in-package :cl-csv)

(defun read-into-buffer-until (buffer stream
                               &key (nl #\newline) partial-newline-match-index
                               &aux
                               (c #\null)
                               (nl-idx (or partial-newline-match-index -1))
                               (nl-len (etypecase nl
                                         (string (length nl))
                                         (character 1)))
                               (nl-len-1 (- nl-len 1))
                               (buffer-len (length buffer)))
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
          (return-from read-into-buffer-until
            (values i nil nl-idx))))
    (setf (schar buffer i) c)
    ;; got the nl
    (when (= nl-len-1 nl-idx)
      (return-from read-into-buffer-until
        (values (+ 1 i) t nl-idx))))
  (values buffer-len nil nl-idx))