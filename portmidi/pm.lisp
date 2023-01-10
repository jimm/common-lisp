(in-package :cl-user)
(defpackage :pm
  (:use :portmidi :common-lisp :sb-alien)
  (:export :channel :message
           :message-status :message-data1 :message-data2
           :device-name :device-input? :device-output? :device-open?
           :device-open-input :device-open-output :device-close
           :virtual-open-input :virtual-open-output :virtual-close
           :devices :print-devices))
(in-package :pm)

(defun channel (chan)
  "Given a channel number 0-15, returns a bit flag useful for
`portmidi:set-channel-mask'."
  (ash 1 chan))

(defun message (status data1 data2)
  "Given three data bytes, returns a `PmMessage'."
  (logior
   (ash (logand data2  #xff) 16)
   (ash (logand data1  #xff)  8)
        (logand status #xff)))

(defun message-status (msg)
  "Extracts status byte from a `PmMessage'."
  (logand msg #xff))

(defun message-data1 (msg)
  "Extracts data1 byte from a `PmMessage'."
  (logand (ash msg -8) #xff))

(defun message-data2 (msg)
  "Extracts data2 byte from a `PmMessage'."
  (logand (ash msg -16) #xff))

(defun device-name (device)
  "Returns the name slot of a `portmidi:device-info'."
  (slot device 'portmidi::name))

(defun device-input? (device)
  "Returns `t' if `device' is an input device."
 (slot device 'portmidi::input))

(defun device-output? (device)
  "Returns `t' if `device' is an input device."
  (slot device 'portmidi::output))

(defun device-open? (device)
  "Returns `t' if `device' is open."
  (slot device 'portmidi::opened))

(defun -device-open-io (num open-func last-arg-p)   ; internal
  "Opens an input or output device and returns the `PmStream' struct. On
error, calls `error'."
  (let* ((args (append (list num nil 1024 nil nil)
                       (if last-arg-p (list 0) ())))
         (vals (multiple-value-list
                (apply open-func args)))
         (err (first vals))
         (stream (second vals)))
    (unless (zerop err)
      (error (portmidi:get-error-text err)))
    stream))

(defun device-open-input (num)
  "Given a device number, opens that input device and returns a `PmStream'
struct. On error, calls `error'."
  (-device-open-io num #'portmidi:open-input nil))

(defun device-open-output (num)
  "Given a device number, opens that output device and returns a `PmStream'
struct. On error, calls `error'."
  (-device-open-io num #'portmidi:open-output t))

(defun device-close (stream)
  "Closes `PmStream' `stream'."
  (close-stream stream))

(defun -guess-interface-name ()
  "Attempts to guess the MIDI interface name by looking at the return value of
~software-type~. Returns one of \"MMSystem\", \"CoreMIDI\", \"ALSA\", or
~nil~."
  (let ((sys-name (string-downcase (software-version))))
    (cond ((string= (subseq sys-name 0 6) "darwin")
           "CoreMIDI")
          ((string= (subseq sys-name 0 3) "win")
           "MMSystem")
          ((string= (subseq sys-name 0 5) "linux")
           "ALSA")
          (t nil))))

(defun virtual-open-input (name &optional interface)
  "Creates a virtual input with the given name and interface
type (\"MMSystem\", \"CoreMIDI\", or \"ALSA\") and returns the device ID. On
error, calls `error'"
  (let* ((interface-name (or interface (-guess-interface-name)))
         (dev (portmidi:create-virtual-input name interface-name 0)))
    (when (< dev 0)
      (error (portmidi:get-error-text dev)))
    dev))

(defun virtual-open-output (name &optional interface)
  "Opens a virtual output and returns a virtual device id. The first argument
is the name to assign the virtual input. The second optional argument is the
interface type to use. This function attempts to guess which to use based on
the return value of a call to ~(software-type)~. As of this writing,
PortMidi supports \"MMSystem\" for Win32, \"ALSA\" for Linux, and
\"CoreMIDI\" for MacOS.

On error, calls `error'."
  (let* ((interface-name (or interface (-guess-interface-name)))
         (dev (portmidi:create-virtual-output name interface-name 0)))
    (when (< dev 0)
      (error (portmidi:get-error-text dev)))
    dev))

(defun virtual-close (device-id)
  "Deletes a virtual device. On error, calls `error'."
  (let ((err (portmidi:delete-virtual-device device-id)))
    (unless (zerop err)
      (error (portmidi:get-error-text err)))))

(defun devices ()
  "Returns a two-element list. The first element is the list of input
devices, the second is the list of output devices. Each element in thoses
lists is a cons whose car is an integer index used to identify the device to
PortMidi and whose cdr is the device struct."
  (let ((inputs ())
        (outputs ()))
      (dotimes (i (portmidi:count-devices))
        (let ((dev (portmidi:get-device-info i)))
          (when (device-input? dev)
            (setq inputs (cons (cons i dev) inputs)))
          (when (device-output? dev)
            (setq outputs (cons (cons i dev) outputs)))))
      (list (reverse inputs) (reverse outputs))))

(defun print-devices ()
  "Writes input and output device numbers and names to stdout, and indicates
if each is open already."
  (let* ((inputs-and-outputs (devices))
         (inputs (first inputs-and-outputs))
         (outputs (second inputs-and-outputs)))
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
      (list-devices "Inputs" inputs)
      (list-devices "Outputs" outputs)))
  nil)
