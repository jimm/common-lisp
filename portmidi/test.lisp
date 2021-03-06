;;; Assumes (pm-initialize) has already been called. It is called from
;;; within load.lisp.

(defun device-info ()
  (let ((inputs ())
        (outputs ()))
    (flet ((list-devices (title indexes-and-devices)
                         (format t "~A:~%" title)
                         (mapcar (lambda (index-and-device)
                                   (let ((index (car index-and-device))
                                         (dev (cdr index-and-device)))
                                     (format t "   ~A: ~A~A~%"
                                             index
                                             (pm:device-name dev)
                                             (if (pm:device-open? dev)
                                                 " (open)"
                                               ""))))
                                 indexes-and-devices)))
      (dotimes (i (portmidi:count-devices))
        (let ((dev (portmidi:get-device-info i)))
          (when (pm:device-input? dev)
            (setq inputs (cons (cons i dev) inputs)))
          (when (pm:device-output? dev)
            (setq outputs (cons (cons i dev) outputs)))))
      (list-devices "Inputs" (reverse inputs))
      (list-devices "Outputs" (reverse outputs)))))

(defparameter *midi-through-thread-running* nil)

;; Don't call this function directly. Call `midi-through-start` and
;; `midi-through-stop`.
(defun midi-through-thread ()
  (let ((in-stream (pm:device-open-input 1))
        (out-stream (pm:device-open-output 4))
        (buffer (make-alien (array (struct portmidi:event) 1024))))
    (loop
     while *midi-through-thread-running*
     do (progn
          (loop while (not (portmidi:poll in-stream)))
          (let* ((read-vals (multiple-value-list
                             (portmidi:midi-read in-stream buffer 1024)))
                 (num-events (car read-vals)))
            (portmidi:midi-write out-stream buffer num-events))))))

(defun midi-through-start ()
  (setf *midi-through-thread-running* t)
  (sb-thread:make-thread (lambda () (midi-through-thread))))

(defun midi-through-stop ()
  (setf *midi-through-thread-running* nil))
