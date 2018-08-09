;; (in-package :cl-user)
;; (defpackage :monoseq
;;   (:use :portmidi)
;;   (:export :run
;;            :random :repeat))
;; (in-package :monoseq)

(defstruct note
  (pitch 64 :type integer)
  (duration 1/4 :type rational)
  (velocity 127 :type integer)
  (off-velocity 127 :type integer))

(defstruct monoseq
  (beats-per-measure 4 :type integer)
  (beat-unit 4 :type rational)
  (tempo 120 :type integer)
  (channel 1 :type integer)
  (notes '() :type list)
  (playing-notes (make-hash-table :test #'equal)) :type hash-table)

(defun report-if-error (err)
  (unless (zerop err)
    (format t "~A~%" (portmidi:get-error-text err))))

(defun seq-note-on (pm-output mseq note)
  (let ((pitch (note-pitch note)))
    (setf (gethash note (monoseq-playing-notes mseq)) note)
    (report-if-error
     (portmidi:midi-write-short
      pm-output 0
      (pm:message (+ #x90 (monoseq-channel mseq))
                  pitch
                  (note-velocity note))))))

(defun seq-note-off (pm-output mseq note)
  (let ((pitch (note-pitch note)))
    (report-if-error
     (portmidi:midi-write-short
      pm-output 0
      (pm:message (+ #x80 (monoseq-channel mseq))
                  pitch
                  (note-off-velocity note)))))
    (remhash note (monoseq-playing-notes mseq)))

(defun monoseq-turn-off-playing-notes (pm-output mseq)
  (maphash #'(lambda (note _)
               (report-if-error
                (portmidi:midi-write-short
                 pm-output 0
                 (pm:message (+ #x80 (monoseq-channel mseq))
                             (note-pitch note)
                             (note-velocity note)))))
           (monoseq-playing-notes mseq)))

(defun run (pm-output mseq)
  (let ((beat-secs (* (monoseq-beat-unit mseq) (/ 60.0 (monoseq-tempo mseq)))))
    (loop for note in (monoseq-notes mseq)
       do (progn
            (seq-note-on pm-output mseq note)
            (sleep (* beat-secs (note-duration note)))
            (seq-note-off pm-output mseq note))))
  ;; not necessary; an example
  (monoseq-turn-off-playing-notes pm-output mseq))

;; sample
(defun monoseq-test ()
  (let ((my-seq (make-monoseq))
        (test-output (pm:device-open-output 2))) ; SimpleSynth virtual input
    (loop for pitch in (reverse '(40 42 44 45 47 49 51 52))
       do
         (setf (monoseq-notes my-seq) (cons (make-note :pitch pitch) (monoseq-notes my-seq))))
    (run test-output my-seq)
    (report-if-error (pm:device-close test-output))))
