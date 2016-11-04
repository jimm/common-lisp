(cl:defpackage "PORTMIDI" (:use "SB-ALIEN" "SB-C-CALL"))
(load-shared-object "libportmidi.dylib")

;;; TODO (declaim (inline pm-whatever)) for efficiency

(define-alien-type nil
  (struct pm-device-info
          (struct-version int)
          (interface c-string)
          (name c-string)
          (input (boolean 32))
          (output (boolean 32))
          (opened (boolean 32))))

(define-alien-type nil
  (struct pm-event
          (message int)
          (timestamp int)))

(define-alien-routine ("Pm_Initialize" pm-initialize)
  int)                                  ; return type

(define-alien-routine ("Pm_Terminate" pm-terminate)
  int)                                  ; return type

(define-alien-routine ("Pm_HasHostError" pm-host-error?)
  int
  (stream (* long)))

(define-alien-routine ("Pm_GetErrorText" pm-get-error-text)
  c-string
  (errnum int))

(define-alien-routine ("Pm_CountDevices" pm-count-devices)
  int)

(define-alien-routine ("Pm_GetDefaultInputDeviceID" pm-get-default-input-device-id)
  int)

(define-alien-routine ("Pm_GetDefaultOutputDeviceID" pm-get-default-output-device-id)
  int)

(define-alien-routine ("Pm_GetDeviceInfo" pm-get-device-info)
  (* (struct pm-device-info))
  (device-id int))

;;; (pm-open-input device-num nil 0 nil nil)
(define-alien-routine ("Pm_OpenInput" pm-open-input)
  int
  (stream long :out)                    ; do not pass in
  (input-device int)
  (input-driver-info (* int))           ; nil
  (buffer-size int)
  (time-proc (* int))                   ; nil
  (time-info (* int)))                  ; nil

;;; (pm-open-output device-num nil 0 nil nil 0)
(define-alien-routine ("Pm_OpenOutput" pm-open-output)
  int
  (stream long :out)                    ; do not pass in
  (output-device int)
  (output-driver-info (* int))          ; nil
  (buffer-size int)
  (time-proc (* int))                   ; nil
  (time-info (* int))                   ; nil
  (latency int))

(define-alien-routine ("Pm_SetFilter" pm-set-filter)
  int
  (stream (* long))
  (filters-bitmask int))

(define-alien-routine ("Pm_SetChannelMask" pm-set-channel-mask)
  int
  (stream (* long))
  (bitmask int))

(define-alien-routine ("Pm_Abort" pm-abort)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Close" pm-close)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Synchronize" pm-synchronize)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Read" pm-read)
  int
  (stream (* long))
  (buffer (* (array (struct pm-event))))
  (length int))

(define-alien-routine ("Pm_Poll" pm-poll)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Write" pm-write)
  int
  (stream (* long))
  (buffer (* (struct pm-event)))
  (length int))

(define-alien-routine ("Pm_WriteShort" pm-write-short)
  int
  (stream (* long))
  (when-tstamp int)
  (msg int))

(define-alien-routine ("Pm_WriteSysEx" pm-write-sysex)
  int
  (stream (* long))
  (when-tstamp int)
  (msg (array unsigned-char)))

(defun pm-channel (chan) (ash 1 chan))

(defun pm-message (status data1 data2)
  (logior
   (ash (logand data2  15) 16)
   (ash (logand data1  15)  8)
        (logand status 15)))

(defun pm-message-status (msg) (logand      msg         15))
(defun pm-message-data1 (msg)  (logand (ash msg (-  8)) 15))
(defun pm-message-data2 (msg)  (logand (ash msg (- 16)) 15))

(defun pm-device-name (device) (slot device 'name))
(defun pm-device-input? (device) (slot device 'input))
(defun pm-device-output? (device) (slot device 'output))
(defun pm-device-open? (device) (slot device 'opened))
