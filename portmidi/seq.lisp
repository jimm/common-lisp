;; (in-package :cl-user)
;; (defpackage :seq
;;   (:use :portmidi)
;;   (:export :run))
;; (in-package :seq)

(defstruct note
  (pitch 64 :type integer)              ; negative pitch == rest; see make-rest
  (duration 1/4 :type rational)
  (velocity 127 :type integer)
  (off-velocity 127 :type integer))

(defun make-rest (&rest args)
  (apply #'make-note (cons :pitch (cons -1 args))))

(defstruct seq
  (name "Unnamed" :type string)
  (beats-per-measure 4 :type integer)
  (beat-unit 4 :type rational)
  (tempo 120 :type integer)
  (channel 1 :type integer)
  (notes '() :type list)
  (playing-notes (make-hash-table :test #'equal)) :type hash-table)

(defun beat-length-seconds (mseq)
  (* (seq-beat-unit mseq) (/ 60.0 (seq-tempo mseq))))

(defun report-if-error (err)
  (unless (zerop err)
    (format t "~A~%" (portmidi:get-error-text err))))

(defun note-on (pm-output mseq note)
  (let ((pitch (note-pitch note)))
    (unless (minusp pitch)
      (setf (gethash note (seq-playing-notes mseq)) note)
      (report-if-error
       (portmidi:midi-write-short
        pm-output 0
        (pm:message (+ #x90 (seq-channel mseq))
                    pitch
                    (note-velocity note)))))))

(defun note-off (pm-output mseq note)
  (let ((pitch (note-pitch note)))
    (unless (minusp pitch)
      (report-if-error
       (portmidi:midi-write-short
        pm-output 0
        (pm:message (+ #x80 (seq-channel mseq))
                    pitch
                    (note-off-velocity note)))))
    (remhash note (seq-playing-notes mseq))))

(defun seq-turn-off-playing-notes (pm-output mseq)
  (maphash #'(lambda (note _)
               (report-if-error
                (portmidi:midi-write-short
                 pm-output 0
                 (pm:message (+ #x80 (seq-channel mseq))
                             (note-pitch note)
                             (note-velocity note)))))
           (seq-playing-notes mseq)))

(defun run (pm-output mseq)
  (let ((beat-secs (beat-length-seconds mseq)))
    (loop for note in (seq-notes mseq)
       do (progn
            (note-on pm-output mseq note)
            (sleep (* beat-secs (note-duration note)))
            (note-off pm-output mseq note))))
  ;; not necessary; an example
  (seq-turn-off-playing-notes pm-output mseq))

;; sample
(defun seq-test ()
  (let ((my-seq (make-seq))
        (test-output (pm:device-open-output 2))) ; SimpleSynth virtual input
    (loop for pitch in (reverse '(40 42 44 45 47 49 51 52))
       do
         (setf (seq-notes my-seq) (cons (make-note :pitch pitch) (seq-notes my-seq))))
    (run test-output my-seq)
    (report-if-error (pm:device-close test-output))))
