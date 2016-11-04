;;; Assumes (pm-initialize) has already been called. It is called from
;;; within load.lisp.

(defun device-info ()
  (let ((inputs ())
        (outputs ()))
    (flet ((list-devices (title devices)
                         (format t "~A:~%" title)
                         (mapcar (lambda (dev)
                                   (format t "~A~A~%"
                                           (pm-device-name dev)
                                           (if (pm-device-open? dev)
                                               " (open)"
                                             "")))
                                 devices)))
      (dotimes (i (pm-count-devices))
        (let ((dev (pm-get-device-info i)))
          (when (pm-device-input? dev)
            (setq inputs (cons dev inputs)))
          (when (pm-device-output? dev)
            (setq outputs (cons dev outputs)))))
      (list-devices "Inputs" inputs)
      (list-devices "Outputs" outputs))))

(defvar *pm-bufsize* 1024)

(defvar in-stream nil)
(defvar out-stream nil)
(defvar in-vals nil)
(defvar out-vals nil)
(setq in-vals (multiple-value-list  (pm-open-input  1 nil *pm-bufsize* nil nil)))
(setq out-vals (multiple-value-list (pm-open-output 4 nil *pm-bufsize* nil nil 0)))

(defun midi-through ()
  (let* ((in-vals (multiple-value-list
                   (pm-open-input 1 nil 1024 nil nil)))
         (out-vals (multiple-value-list
                    (pm-open-output 4 nil 1024 nil nil 0)))
         (in-err (car in-vals))
         (out-err (car in-vals))
         (in-stream (cadr in-vals))
         (out-stream (cadr out-vals)))
    (when (not (zerop in-err))
      (error (pm-get-error-text in-err)))
    (when (not (zerop out-err))
      (error (pm-get-error-text out-err)))
    (while t
      (while (not (pm-poll in-stream))) ; nop
      (let* ((buffer (make-alien (array (struct pm-event) 1024)))
             (read-vals (multiple-value-list
                         (pm-read in-stream buffer 1024)))
             (num-events (car read-vals)))
        (pm-write out-stream buffer num-events)))))
