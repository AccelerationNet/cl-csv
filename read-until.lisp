(in-package :cl-csv)

(defun read-into-buffer-until (buffer stream &optional (nl #\newline)
                               &aux (nl-idx -1) (c #\null)
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
  (macrolet ((matching-newline? ()
               `(let ((new-idx (+ 1 nl-idx)))
                 (declare (type fixnum new-idx))
                 (if (char= (etypecase nl
                              (string (char nl new-idx))
                              (character nl))
                            c)
                     (setf nl-idx new-idx)
                     (setf nl-idx -1))
                 (= nl-len-1 nl-idx))))
    (dotimes (i buffer-len)
      (setf c (read-char stream nil *eof-char*))
      (when (char= *eof-char* c)
        (if (zerop i)
            (error 'end-of-file :stream stream)
            (return-from read-into-buffer-until
              i)))
      (setf (schar buffer i) c)
      ;; got the nl
      (when (matching-newline?)
        (return-from read-into-buffer-until
          (values (- i nl-idx) t))))
    buffer-len))