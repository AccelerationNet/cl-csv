(in-package :cl-csv)

;; Original Author: Dimitri Fontaine, dimitri@github, dim@#lisp
;; https://github.com/AccelerationNet/cl-csv/issues/13

(defvar *buffer-size* 512)

(defun concatenate-vectors (total-length vectors)
  "Given a list of VECTORS containing LENGTH octets in total, return a
   single vector containing the same octets in the same order."
  (let ((vector (make-string total-length)))
    (loop for start = 0 then (+ start (length sub-vector))
       for sub-vector in vectors
       do (replace vector (the (simple-string) sub-vector)
                   :start1 start))
    vector))

(defun read-until (stream &optional (until #\Newline))
  "Read from STREAM until the UNTIL marker is found. When UNTIL is #\Newline
   read-until should be equivalent to read-line."
  (declare (type (or character string) until))
  (let* ((buffers)                       ; list of buffers to concatenate
         (buffer    (make-string *buffer-size*)) ; current buffer
         (total-len 0)                  ; how many chars we concatenate
         (position  0)                  ; in the current buffer
         (state     nil)                ;
         (end-pos   0)                  ; position when reading the end string
         (end-len   (when (stringp until) (length until)))
         (ending    (when end-len         (make-string end-len))))
    (labels ((%next-state (state char until)
               (typecase until
                 (character
                  (cond ((or (char= until char) (char= char *eof-char*))
                         :done)
                        (t
                         :collecting)))

                 (string
                  (cond ((and (eq state :maybe-ending)
                              (= (+ end-pos 1) end-len)
                              (char= (aref until end-pos) char))
                         :done)
                        ((char= (aref until end-pos) char)
                         :maybe-ending)
                        ((eq state :maybe-ending)
                         :inject-ending)
                        (t
                         :collecting)))))
             (collect-char (char)
               ;; should we prepare another buffer?
               (when (= position *buffer-size*)
                 (push buffer buffers)
                 (setf buffer (make-string *buffer-size*) position 0))

               (setf (aref buffer position) char)
               (incf position)
               (incf total-len))

             (collect-string (string &optional (start 0) end)
               (loop for p from start to (or end (length string))
                  do (collect-char (aref string p)))))

      (push buffer buffers)
      (let ((until (if (and (stringp until) (= 1 (length until)))
                       (aref until 0)
                       until)))
        (loop
          for c = (read-char stream nil *eof-char*)
          do (case (setf state (%next-state state c until))
               (:collecting    (collect-char c))

               (:maybe-ending  (setf (aref ending end-pos) c)
                (incf end-pos))

               (:inject-ending (collect-string ending 0 end-pos)
                (collect-char c)
                (setf end-pos 0)))
          until (eq state :done)
          finally (return (if (plusp total-len)
                              (concatenate-vectors total-len (reverse buffers))
                              (signal 'end-of-file))))))))