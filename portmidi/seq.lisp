;; (in-package :cl-user)
;; (defpackage :seq
;;   (:use :portmidi)
;;   (:export :song-play))
;; (in-package :seq)

;;; ================ events ================

(defstruct note
  (pitch 64 :type integer)              ; negative pitch == rest; see make-rest
  (duration 1/4 :type rational)
  (velocity 64 :type integer)
  (off-velocity 64 :type integer))

(defun make-rest (&rest args)
  (apply #'make-note (cons :pitch (cons -1 args))))

;;; ================ track ================

(defstruct track
  (name "Unnamed" :type string)
  (channel 1 :type integer)
  (notes '() :type list)
  (playing-notes (make-hash-table :test #'equal) :type hash-table))

(defun track-remember-note (track note)
  (setf (gethash note (track-playing-notes track)) note))

(defun track-forget-note (track note)
  (remhash note (track-playing-notes track)))

;;; ================ song ================

(defstruct song
  (name "Unnamed" :type string)
  (beats-per-measure 4 :type integer)
  (beat-unit 4 :type rational)
  (tempo 120 :type integer)
  (tracks '() :type list))

(defun beat-length-seconds (s)
  (* (song-beat-unit s) (/ 60.0 (song-tempo s))))

;;; ================ playing ================

(defun report-if-error (err)
  (unless (zerop err)
    (format t "~A~%" (portmidi:get-error-text err))))

(defun play-note-event (pm-output status channel pitch velocity)
  (unless (minusp pitch)
    (report-if-error
     (portmidi:midi-write-short
      pm-output 0
      (pm:message (+ status channel) pitch velocity)))))

(defun play-note-on (pm-output channel note)
  (play-note-event pm-output #x90 channel
                   (note-pitch note) (note-velocity note)))

(defun play-note-off (pm-output channel note)
  (play-note-event pm-output #x80 channel
                   (note-pitch note) (note-off-velocity note)))

(defun song-play (pm-output s)
  (let* ((beat-secs (beat-length-seconds s))
         ;; for now, in this test code, seq must have one track
         (track (first (song-tracks s)))
         (channel (track-channel track)))
    (loop for event in (track-notes track)
       do (progn
            (track-remember-note track event)
            (play-note-on pm-output channel event)
            (sleep (* beat-secs (note-duration event)))
            (play-note-off pm-output channel event)
            (track-forget-note track event)))
    (maphash (lambda (event _)
               (declare (ignore _))
               (play-note-off pm-output channel event))
             (track-playing-notes track))))

;; sample
(defun song-test ()
  (let* ((my-track (make-track))
         (my-seq (make-song :tracks (list my-track)))
         (test-output (pm:device-open-output 2))) ; SimpleSynth virtual input
    (loop for pitch in (reverse '(40 42 44 45 47 49 51 52))
       do
         (setf (track-notes my-track)
               (cons (make-note :pitch pitch) (track-notes my-track))))
    (setf (track-notes my-track) (append (track-notes my-track)
                                         (reverse (track-notes my-track))))
    (song-play test-output my-seq)
    (report-if-error (pm:device-close test-output))))
