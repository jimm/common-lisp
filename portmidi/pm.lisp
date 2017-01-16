(defpackage #:pm
  (:use #:portmidi #:common-lisp #:sb-alien)
  (:export #:channel #:message
           #:message-status #:message-data1 #:message-data2
           #:device-name #:device-input? #:device-output? #:device-open?
           #:device-open-input #:device-open-output #:list-devices))

(in-package #:pm)

(defun channel (chan) (ash 1 chan))

(defun message (status data1 data2)
  (logior
   (ash (logand data2  #xff) 16)
   (ash (logand data1  #xff)  8)
           (logand status #xff)))

(defun message-status (msg) (logand         msg         #xff))
(defun message-data1 (msg)  (logand (ash msg (-  8)) #xff))
(defun message-data2 (msg)  (logand (ash msg (- 16)) #xff))

(defun device-name (device) (slot device 'portmidi::name))
(defun device-input? (device) (slot device 'portmidi::input))
(defun device-output? (device) (slot device 'portmidi::output))
(defun device-open? (device) (slot device 'portmidi::opened))

(defun device-open-io (num open-func)   ; internal
  (let* ((vals (multiple-value-list
                (funcall open-func num nil 1024 nil nil)))
         (err (car vals))
         (stream (cadr vals)))
    (when (not (zerop err))
      (error (portmidi:get-error-text err)))
    stream))

(defun device-open-input (num)
  (device-open-io num #'portmidi:open-input))

(defun device-open-output (num)
  (device-open-io num #'portmidi:open-output))

(defun list-devices ()
  (let ((inputs ())
        (outputs ()))
    (flet ((list-devices (title indexes-and-devices)
                         (format t "~A:~%" title)
                         (mapcar (lambda (index-and-device)
                                   (let ((index (car index-and-device))
                                         (dev (cdr index-and-device)))
                                     (format t "   ~A: ~A~A~%"
                                             index
                                             (device-name dev)
                                             (if (device-open? dev)
                                                 " (open)"
                                               ""))))
                                 indexes-and-devices)))
      (dotimes (i (portmidi:count-devices))
        (let ((dev (portmidi:get-device-info i)))
          (when (device-input? dev)
            (setq inputs (cons (cons i dev) inputs)))
          (when (device-output? dev)
            (setq outputs (cons (cons i dev) outputs)))))
      (list-devices "Inputs" (reverse inputs))
      (list-devices "Outputs" (reverse outputs))))
  nil)
