;; (in-package :cl-user)
;; (defpackage :seq
;;   (:use :portmidi)
;;   (:export :song-play))
;; (in-package :seq)

;;; ================ event ================

(defstruct event
  (time 0.0 :type float)
  (midi 0 :type integer))

;;; ================ notes ================

(defstruct note
  (pitch 64 :type integer)              ; negative pitch == rest; see make-rest
  (duration 1/4 :type rational)
  (velocity 64 :type integer)
  (off-velocity 64 :type integer))

(defun make-rest (&rest args)
  (apply #'make-note (cons :pitch (cons -1 args))))

;; (defun note-secs (

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

(defun secs-per-beat (s)
  "Returns the length of one beat in seconds."
  (/ 60.0 (song-tempo s)))

(defun duration->secs (s note-start-time note-duration)
  "Given a `note-start-time' in seconds and a `note-duration', returns the
number of seconds of the note's duration."
  (declare (ignore note-start-time))    ; don't handle temp changes yet
  (* (secs-per-beat s) (song-beat-unit s) note-duration))

;;; ================ note-to-event conversion ================

(defun track-notes-to-events (s track)
  "Converts the notes of `track' into a list of `event' structs, using
`secs-func' to convert each note's duration into a number of seconds."
  (let ((i 0.0)
        (duration nil))
    (loop for note in (track-notes track)
       do (setf duration (duration->secs s i (note-duration note)))
       unless (minusp (note-pitch note))
       collect
         (make-event :time i
                     :midi (pm:message (+ #x90 (track-channel track))
                                       (note-pitch note)
                                       (note-velocity note)))
       unless (minusp (note-pitch note))
       collect
         (make-event :time (+ i duration)
                     :midi (pm:message (+ #x80 (track-channel track))
                                       (note-pitch note)
                                       (note-off-velocity note)))
       do (setf i (+ i duration)))))

(defun song-events (s)
  "Converts all notes in all tracks in song `s` into a single list of events."
  (sort
   (mapcan (lambda (track) (track-notes-to-events s track))
           (song-tracks s))
   #'< :key #'event-time))

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
  ;; TODO remember note (channel+pitch) when it starts playing, forget if
  ;; it's being turned off. Play remembered note-offs at end.
  (let ((i 0.0))
    (mapc (lambda (event)
            (let ((start-time (event-time event)))
              (sleep (- start-time i))
              (setf i start-time)
              (report-if-error
               (portmidi:midi-write-short pm-output 0 (event-midi event)))))
          (song-events s))))

;; ================ sample ================

;; Optional args are song slots, e.g. (song-test :tempo 150)
(defun make-test-song (&rest seq-args)
  (let* ((new-track (make-track))
         (new-seq (apply #'make-song
                         (append seq-args (list :tracks (list new-track))))))
    (loop for pitch in (reverse '(40 42 44 45 47 49 51 52))
       do
         (setf (track-notes new-track)
               (cons (make-note :pitch pitch :duration 1/8)
                     (track-notes new-track))))
    (setf (track-notes new-track) (append (track-notes new-track)
                                         (reverse (track-notes new-track))))
    new-seq))

;; Optional args are song slots, e.g. (song-test :tempo 150)
(defun song-test (&rest seq-args)
  (let ((my-seq (apply #'make-test-song seq-args))
        (test-output (pm:device-open-output 2))) ; SimpleSynth virtual input
    (song-play test-output my-seq)
    (report-if-error (pm:device-close test-output))))
