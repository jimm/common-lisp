(cl:defpackage "PORTMIDI"
               (:use "SB-ALIEN" "SB-C-CALL")
               (:export "DEVICE-INFO" "EVENT"
                        "INITIALIZE" "TERMINATE" "HOST-ERROR?" "GET-ERROR-TEXT"
                        "COUNT-DEVICES" "GET-DEFAULT-INPUT-DEVICE-ID"
                        "GET-DEFAULT-OUTPUT-DEVICE-ID" "GET-DEVICE-INFO"
                        "OPEN-INPUT" "OPEN-OUTPUT" "SET-FILTER" "SET-CHANNEL-MASK"
                        "ABORT-WRITE" "CLOSE-STREAM" "SYNCHRONIZE" "MIDI-READ"
                        "POLL" "MIDI-WRITE" "MIDI-WRITE-SHORT" "MIDI-WRITE-SYSEX"))
(cl:in-package "PORTMIDI")

(load-shared-object "libportmidi.dylib")

;;; TODO (declaim (inline whatever)) for efficiency

(define-alien-type device-info
  (struct device-info
          (struct-version int)
          (interface c-string)
          (name c-string)
          (input (boolean 32))
          (output (boolean 32))
          (opened (boolean 32))))

(define-alien-type event
  (struct event
          (message int)
          (timestamp int)))

(define-alien-routine ("Pm_Initialize" initialize)
  int)                                  ; return type

(define-alien-routine ("Pm_Terminate" terminate)
  int)                                  ; return type

(define-alien-routine ("Pm_HasHostError" host-error?)
  int
  (stream (* long)))

(define-alien-routine ("Pm_GetErrorText" get-error-text)
  c-string
  (errnum int))

(define-alien-routine ("Pm_CountDevices" count-devices)
  int)

(define-alien-routine ("Pm_GetDefaultInputDeviceID" get-default-input-device-id)
  int)

(define-alien-routine ("Pm_GetDefaultOutputDeviceID" get-default-output-device-id)
  int)

(define-alien-routine ("Pm_GetDeviceInfo" get-device-info)
  (* (struct device-info))
  (device-id int))

;;; (open-input device-num nil 0 nil nil)
(define-alien-routine ("Pm_OpenInput" open-input)
  int
  (stream long :out)                    ; do not pass in
  (input-device int)
  (input-driver-info (* int))           ; nil
  (buffer-size int)
  (time-proc (* int))                   ; nil
  (time-info (* int)))                  ; nil

;;; (open-output device-num nil 0 nil nil 0)
(define-alien-routine ("Pm_OpenOutput" open-output)
  int
  (stream long :out)                    ; do not pass in
  (output-device int)
  (output-driver-info (* int))          ; nil
  (buffer-size int)
  (time-proc (* int))                   ; nil
  (time-info (* int))                   ; nil
  (latency int))

(define-alien-routine ("Pm_SetFilter" set-filter)
  int
  (stream (* long))
  (filters-bitmask int))

(define-alien-routine ("Pm_SetChannelMask" set-channel-mask)
  int
  (stream (* long))
  (bitmask int))

(define-alien-routine ("Pm_Abort" abort-write)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Close" close-stream)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Synchronize" synchronize)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Read" midi-read)
  int
  (stream (* long))
  (buffer (* (array (struct event))))
  (length int))

(define-alien-routine ("Pm_Poll" poll)
  int
  (stream (* long)))

(define-alien-routine ("Pm_Write" midi-write)
  int
  (stream (* long))
  (buffer (* (struct event)))
  (length int))

(define-alien-routine ("Pm_WriteShort" midi-write-short)
  int
  (stream (* long))
  (when-tstamp int)
  (msg int))

(define-alien-routine ("Pm_WriteSysEx" midi-write-sysex)
  int
  (stream (* long))
  (when-tstamp int)
  (msg (array unsigned-char)))
