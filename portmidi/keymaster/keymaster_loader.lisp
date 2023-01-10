;; assumes "load.lisp" is already loaded
;; (load "instrument")

(defun keymaster-data (&key instruments messages triggers songs set-lists)
  (let ((h (make-hash-table)))
    (setf (gethash 'instruments h) instruments)
    (setf (gethash 'messages h) messages)
    (setf (gethash 'triggers h) triggers)
    (setf (gethash 'songs h) songs)
    (setf (gethash 'set-lists h) set-lists)
    (format t "~a~%" h)
    (format t "~a~%" (gethash 'instruments h))
    h))
