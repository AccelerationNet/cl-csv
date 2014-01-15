(in-package :cl-csv)

(defun read-into-buffer-until (buffer stream &optional (nl #\newline)
                               &aux (cnt 0) (nl-len (etypecase nl
                                                      (string (length nl))
                                                      (character 1))))
  (handler-bind
      ((end-of-file
         (lambda (c) (declare (ignore c))
           (when (plusp cnt)
             (return-from read-into-buffer-until
               cnt)))))
    (iter (for i from 0 below (length buffer))
      (setf (aref buffer i) (read-char stream)
            cnt (+ 1 i))
      ;; got the nl
      (let ((start (- cnt nl-len)))
        (when (and (plusp start)
                   (iter (for j from start to i)
                     (for k from 0)
                     (for nlc = (etypecase nl
                                  (string (char nl k))
                                  (character nl)))
                     (always (char= (aref buffer j) nlc))))
          (return-from read-into-buffer-until
            (values start t))
          ))))
  cnt)