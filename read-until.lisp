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

(defun fast-read-into-buffer-until (buffer stream &optional (nl #\newline))
  #+sbcl
  (return-from fast-read-into-buffer-until
    (if (sb-impl::ansi-stream-cin-buffer stream)
        (%fast-read-into-buffer-until buffer stream)
        (read-into-buffer-until buffer stream nl)))
  #-sbcl
  (read-into-buffer-until buffer stream nl))

#+sbcl ;; copied and mutated from sb-impl::ansi-stream-read-line-from-frc-buffer
(defun %fast-read-into-buffer-until (buffer stream &optional (nl #\newline))
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type (simple-array character) buffer))
  (sb-int:prepare-for-fast-read-char stream
    (declare (ignore sb-impl::%frc-method%))
    (let ((b-len (length buffer)) (b-idx 0))
      (declare (type fixnum b-len b-idx))
      (labels ((refill-buffer ()
                 (prog1
                     (sb-int:fast-read-char-refill stream nil nil)
                   (setf sb-impl::%frc-index%
                         (sb-kernel:ansi-stream-in-index sb-impl::%frc-stream%))))
               (newline-position ()
                 (etypecase nl
                   (simple-string (search nl
                                          (the (simple-array character (*))
                                               sb-impl::%frc-buffer%)
                                          :test #'char= :start2 sb-impl::%frc-index%))
                   (character (position nl (the (simple-array character (*))
                                                sb-impl::%frc-buffer%)
                                        :test #'char=
                                        :start sb-impl::%frc-index%))))
               (return-partial-buffer (pos &optional nl?)
                 (replace buffer sb-impl::%frc-buffer%
                          :start1 b-idx
                          :start2 sb-impl::%frc-index%
                          :end2 pos)
                 (setf sb-impl::%frc-index% (1+ pos))
                 (sb-int:done-with-fast-read-char)
                 (return-from %fast-read-into-buffer-until
                   (values (+ 1 b-idx) nl? )))

               (return-full-buffer (&aux (remaining (the fixnum (- b-len b-idx)))
                                    (end (+ sb-impl::%frc-index% remaining))
                                    (end-l (1+ end)))
                 
                 (replace buffer sb-impl::%frc-buffer%
                          :start1 b-idx
                          :start2 sb-impl::%frc-index%
                          :end2 end)
                 (setf sb-impl::%frc-index% end-l)
                 (sb-int:done-with-fast-read-char)
                 (return-from %fast-read-into-buffer-until b-len))

               (copy-into-buffer ()
                 (let* ((end (length sb-impl::%frc-buffer%))
                        (len (- end sb-impl::%frc-index%)))
                   (replace buffer sb-impl::%frc-buffer%
                            :start1 b-idx
                            :start2 sb-impl::%frc-index%
                            :end2 end)
                   (setf b-idx (+ b-idx len))
                   (when (refill-buffer)
                     (return-from %fast-read-into-buffer-until
                       (+ 1 b-idx))))))
        
        (declare (inline return-full-buffer return-partial-buffer copy-into-buffer refill-buffer))
        (when (and (= sb-impl::%frc-index% sb-impl::+ansi-stream-in-buffer-length+)
                   (refill-buffer))
          ;; EOF had been reached before we read anything
          ;; at all. Return the EOF value or signal the error.
          (sb-int:done-with-fast-read-char)
          (error 'end-of-file :stream stream))
        (loop do
          (let* ((pos (newline-position))
                 (line-len (and pos (- pos sb-impl::%frc-index%)))
                 (end (length sb-impl::%frc-buffer%))
                 (len (- end sb-impl::%frc-index%))
                 (new-out-len (+ b-idx len)))
            ;;(when pos (adwutils:spy-break pos new-out-len b-idx buffer))
            (cond
              ;; line fits in our buffer and we have our new line
              ((and pos (<= (+ b-idx line-len) b-len))
               (return-partial-buffer pos t))

              ((< b-len new-out-len)
               (return-full-buffer))

              (T (copy-into-buffer)))))))))