;; (in-package :cl-user)
;; (defpackage :monoseq
;;   (:use :portmidi)
;;   (:export :run
;;            :random :repeat))
;; (in-package :monoseq)

(defstruct monoseq
  (tempo 120 :type integer)
  (channel 1 :type integer)
  (playing-notes (make-hash-table) :type hash-table))

(defun report-if-error (err)
  (unless (zerop err)
    (format t "~A~%" (portmidi:get-error-text err))))

(defun seq-loop (pm-output mseq &rest steps)
  (let ((pause-secs (/ 60.0 (monoseq-tempo mseq))))
    (dotimes (i 3)
    ;; (while t
      (loop for step in steps
         do (progn
              (format t "on ~a~%" step)
              (report-if-error (portmidi:midi-write-short pm-output 0 (pm:message #x90 step 127)))
              (sleep pause-secs)
              (format t "off ~a~%" step)
              (report-if-error (portmidi:midi-write-short pm-output 0 (pm:message #x80 step 127))))))))

;; sample
(defvar my-seq (make-monoseq))
(defvar test-output nil)

(setf test-output (pm:device-open-output 2)) ; SimpleSynth virtual input

(seq-loop test-output my-seq
          40 42 44 45 47 49 51 52)

(report-if-error (pm:device-close test-output))
