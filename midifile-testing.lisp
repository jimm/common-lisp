(ql:quickload "midi")                   ; assumes it has been installed, of course

(defvar mf nil)
(setf mf (midi:read-midi-file "~/src/github/midilib/examples/from_scratch.mid"))

(midi:midifile-format mf)               ; => 1
(midi:midifile-division mf)             ; => 480
(length (midi:midifile-tracks mf))      ; => 2

(defvar t1 (first  (midi:midifile-tracks mf)))
(defvar t2 (second (midi:midifile-tracks mf)))

(first t1)                              ; => <#MIDI::INSTRUMENT_MESSAGE {...}>
(defvar inst (first t1))
