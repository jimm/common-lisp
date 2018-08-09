(in-package :cl-user)
(defpackage :midi-consts
  (:nicknames :mc)
  (:export
   ))
(in-package :midi-consts)

;; ;; MIDI and PatchMaster constants.
;; module PM

;;   ;; Define MIDI note names C0 - B10
;;   (0..10).each { |oct|
;;     {:C => 0, :D => 2, :E => 4, :F => 5, :G => 7, :A => 9, :B => 11}.each { |note,val|
;;       base = (oct+1) * 12 + val
;;       eval <<EOS
;; ;;{note};;{oct} = ;;{base}
;; ;;{note}f;;{oct} = ;;{base - 1}
;; ;;{note}b;;{oct} = ;;{base - 1}
;; ;;{note}s;;{oct} = ;;{base + 1}
;; EOS
;;     }
;;   }

  ;; Number of MIDI channels
(defconstant midi-channels 16)
  ;; Number of note per MIDI channel
(defconstant notes-per-channel 128)

  ;;--
  ;; Standard MIDI File meta event defs.
  ;;++
(defconstant meta-event #xff)
(defconstant meta-seq-num #x00)
(defconstant meta-text #x01)
(defconstant meta-copyright #x02)
(defconstant meta-seq-name #x03)
(defconstant meta-instrument #x04)
(defconstant meta-lyric #x05)
(defconstant meta-marker #x06)
(defconstant meta-cue #x07)
(defconstant meta-midi-chan-prefix #x20)
(defconstant meta-track-end #x2f)
(defconstant meta-set-tempo #x51)
(defconstant meta-smpte #x54)
(defconstant meta-time-sig #x58)
(defconstant meta-patch-sig #x59)
(defconstant meta-seq-specif #x7f)

  ;;--
  ;; Channel messages
  ;;++
  ;; Note, val
(defconstant note-off #x80)
  ;; Note, val
(defconstant note-on #x90)
  ;; Note, val
(defconstant poly-pressure #xA0)
  ;; Controller ;;, val
(defconstant controller #xB0)
  ;; Program number
(defconstant program-change #xC0)
  ;; Channel pressure
(defconstant channel-pressure #xD0)
  ;; LSB, MSB
(defconstant pitch-bend #xE0)

  ;;--
  ;; System common messages
  ;;++
  ;; System exclusive start
(defconstant sysex #xF0)
  ;; Beats from top: LSB/MSB 6 ticks = 1 beat
(defconstant song-pointer #xF2)
  ;; Val = number of song
(defconstant song-select #xF3)
  ;; Tune request
(defconstant tune-request #xF6)
  ;; End of system exclusive
(defconstant eox #xF7)

  ;;--
  ;; System realtime messages
  ;;++
  ;; MIDI clock (24 per quarter note)
(defconstant clock #xF8)
  ;; Sequence start
(defconstant start #xFA)
  ;; Sequence continue
(defconstant continue #xFB)
  ;; Sequence stop
(defconstant stop #xFC)
  ;; Active sensing (sent every 300 ms when nothing else being sent)
(defconstant active-sense #xFE)
  ;; System reset
(defconstant system-reset #xFF)

  ;;--
  ;; Controller numbers
  ;; = 0 - 31 = continuous, MSB
  ;; = 32 - 63 = continuous, LSB
  ;; = 64 - 97 = momentary switches
  ;;++
(defconstant cc-bank-select-msb 0)
(defconstant cc-bank-select cc-bank-select-msb)
(defconstant cc-mod-wheel-msb 1)
(defconstant cc-mod-wheel cc-mod-wheel-msb)
(defconstant cc-breath-controller-msb 2)
(defconstant cc-breath-controller cc-breath-controller-msb)
(defconstant cc-foot-controller-msb 4)
(defconstant cc-foot-controller cc-foot-controller-msb)
(defconstant cc-portamento-time-msb 5)
(defconstant cc-portamento-time cc-portamento-time-msb)
(defconstant cc-data-entry-msb 6)
(defconstant cc-data-entry cc-data-entry-msb)
(defconstant cc-volume-msb 7)
(defconstant cc-volume cc-volume-msb)
(defconstant cc-balance-msb 8)
(defconstant cc-balance cc-balance-msb)
(defconstant cc-pan-msb 10)
(defconstant cc-pan cc-pan-msb)
(defconstant cc-expression-controller-msb 11)
(defconstant cc-expression-controller cc-expression-controller-msb)
(defconstant cc-gen-purpose-1-msb 16)
(defconstant cc-gen-purpose-1 cc-gen-purpose-1-msb)
(defconstant cc-gen-purpose-2-msb 17)
(defconstant cc-gen-purpose-2 cc-gen-purpose-2-msb)
(defconstant cc-gen-purpose-3-msb 18)
(defconstant cc-gen-purpose-3 cc-gen-purpose-3-msb)
(defconstant cc-gen-purpose-4-msb 19)
(defconstant cc-gen-purpose-4 cc-gen-purpose-4-msb)

  ;;--
  ;; [32 - 63] are LSB for [0 - 31]
  ;;++
(defconstant cc-bank-select-lsb (+ 32 cc-bank-select-msb))
(defconstant cc-mod-wheel-lsb (+ 32 cc-mod-wheel-msb))
(defconstant cc-breath-controller-lsb (+ 32 cc-breath-controller-msb))
(defconstant cc-foot-controller-lsb (+ 32 cc-foot-controller-msb))
(defconstant cc-portamento-time-lsb (+ 32 cc-portamento-time-msb))
(defconstant cc-data-entry-lsb (+ 32 cc-data-entry-msb))
(defconstant cc-volume-lsb (+ 32 cc-volume-msb))
(defconstant cc-balance-lsb (+ 32 cc-balance-msb))
(defconstant cc-pan-lsb (+ 32 cc-pan-msb))
(defconstant cc-expression-controller-lsb (+ 32 cc-expression-controller-msb))
(defconstant cc-gen-purpose-1-lsb (+ 32 cc-gen-purpose-1-msb))
(defconstant cc-gen-purpose-2-lsb (+ 32 cc-gen-purpose-2-msb))
(defconstant cc-gen-purpose-3-lsb (+ 32 cc-gen-purpose-3-msb))
(defconstant cc-gen-purpose-4-lsb (+ 32 cc-gen-purpose-4-msb))

  ;;--
  ;; Momentary switches:
  ;;++
(defconstant cc-sustain 64)
(defconstant cc-portamento 65)
(defconstant cc-sustenuto 66)
(defconstant cc-soft-pedal 67)
(defconstant cc-hold-2 69)
(defconstant cc-gen-purpose-5 50)
(defconstant cc-gen-purpose-6 51)
(defconstant cc-gen-purpose-7 52)
(defconstant cc-gen-purpose-8 53)
(defconstant cc-ext-effects-depth 91)
(defconstant cc-tremelo-depth 92)
(defconstant cc-chorus-depth 93)
(defconstant cc-detune-depth 94)
(defconstant cc-phaser-depth 95)
(defconstant cc-data-increment 96)
(defconstant cc-data-decrement 97)
(defconstant cc-nreg-param-lsb 98)
(defconstant cc-nreg-param-msb 99)
(defconstant cc-reg-param-lsb 100)
(defconstant cc-reg-param-msb 101)

  ;;--
  ;; Channel mode message values
  ;;++
  ;; Val 0 == off, #x7f == on
(defconstant cm-reset-all-controllers #x79)
(defconstant cm-local-control #x7A)
(defconstant cm-all-notes-off #x7B) ;; Val must be 0
(defconstant cm-omni-mode-off #x7C) ;; Val must be 0
(defconstant cm-omni-mode-on #x7D)  ;; Val must be 0
(defconstant cm-mono-mode-on #x7E)  ;; Val = # chans
(defconstant cm-poly-mode-on #x7F)  ;; Val must be 0

(defconstant controller-names '(
    "Bank Select (MSB)"
    "Modulation (MSB)"
    "Breath Control (MSB)"
    "3 (MSB)"
    "Foot Controller (MSB)"
    "Portamento Time (MSB)"
    "Data Entry (MSB)"
    "Volume (MSB)"
    "Balance (MSB)"
    "9 (MSB)"
    "Pan (MSB)"
    "Expression Control (MSB)"
    "12 (MSB)", "13 (MSB)", "14 (MSB)", "15 (MSB)"
    "General Controller 1 (MSB)"
    "General Controller 2 (MSB)"
    "General Controller 3 (MSB)"
    "General Controller 4 (MSB)"
    "20 (MSB)", "21 (MSB)", "22 (MSB)", "23 (MSB)", "24 (MSB)", "25 (MSB)"
    "26 (MSB)", "27 (MSB)", "28 (MSB)", "29 (MSB)", "30 (MSB)", "31 (MSB)"

    "Bank Select (LSB)"
    "Modulation (LSB)"
    "Breath Control (LSB)"
    "35 (LSB)"
    "Foot Controller (LSB)"
    "Portamento Time (LSB)"
    "Data Entry (LSB)"
    "Volume (LSB)"
    "Balance (LSB)"
    "41 (LSB)"
    "Pan (LSB)"
    "Expression Control (LSB)"
    "44 (LSB)", "45 (LSB)", "46 (LSB)", "47 (LSB)"
    "General Controller 1 (LSB)"
    "General Controller 2 (LSB)"
    "General Controller 3 (LSB)"
    "General Controller 4 (LSB)"
    "52 (LSB)", "53 (LSB)", "54 (LSB)", "55 (LSB)", "56 (LSB)", "57 (LSB)"
    "58 (LSB)", "59 (LSB)", "60 (LSB)", "61 (LSB)", "62 (LSB)", "63 (LSB)"

    "Sustain Pedal"
    "Portamento"
    "Sostenuto"
    "Soft Pedal"
    "68"
    "Hold 2"
    "70", "71", "72", "73", "74", "75", "76", "77", "78", "79"
    "General Controller 5"
    "Tempo Change"
    "General Controller 7"
    "General Controller 8"
    "84", "85", "86", "87", "88", "89", "90"
    "External Effects Depth"
    "Tremolo Depth"
    "Chorus Depth"
    "Detune (Celeste) Depth"
    "Phaser Depth"
    "Data Increment"
    "Data Decrement"
    "Non-Registered Param LSB"
    "Non-Registered Param MSB"
    "Registered Param LSB"
    "Registered Param MSB"
    "102", "103", "104", "105", "106", "107", "108", "109"
    "110", "111", "112", "113", "114", "115", "116", "117"
    "118", "119", "120"
    "Reset All Controllers"
    "Local Control"
    "All Notes Off"
    "Omni Mode Off"
    "Omni Mode On"
    "Mono Mode On"
    "Poly Mode On"
)

  ;; General MIDI patch names
(defconstant gm-patch-names [
    ;;--
    ;; Pianos
    ;;++
    "Acoustic Grand Piano"
    "Bright Acoustic Piano"
    "Electric Grand Piano"
    "Honky-tonk Piano"
    "Electric Piano 1"
    "Electric Piano 2"
    "Harpsichord"
    "Clavichord"
    ;;--
    ;; Tuned Idiophones
    ;;++
    "Celesta"
    "Glockenspiel"
    "Music Box"
    "Vibraphone"
    "Marimba"
    "Xylophone"
    "Tubular Bells"
    "Dulcimer"
    ;;--
    ;; Organs
    ;;++
    "Drawbar Organ"
    "Percussive Organ"
    "Rock Organ"
    "Church Organ"
    "Reed Organ"
    "Accordion"
    "Harmonica"
    "Tango Accordion"
    ;;--
    ;; Guitars
    ;;++
    "Acoustic Guitar (nylon)"
    "Acoustic Guitar (steel)"
    "Electric Guitar (jazz)"
    "Electric Guitar (clean)"
    "Electric Guitar (muted)"
    "Overdriven Guitar"
    "Distortion Guitar"
    "Guitar harmonics"
    ;;--
    ;; Basses
    ;;++
    "Acoustic Bass"
    "Electric Bass (finger)"
    "Electric Bass (pick)"
    "Fretless Bass"
    "Slap Bass 1"
    "Slap Bass 2"
    "Synth Bass 1"
    "Synth Bass 2"
    ;;--
    ;; Strings
    ;;++
    "Violin"
    "Viola"
    "Cello"
    "Contrabass"
    "Tremolo Strings"
    "Pizzicato Strings"
    "Orchestral Harp"
    "Timpani"
    ;;--
    ;; Ensemble strings and voices
    ;;++
    "String Ensemble 1"
    "String Ensemble 2"
    "SynthStrings 1"
    "SynthStrings 2"
    "Choir Aahs"
    "Voice Oohs"
    "Synth Voice"
    "Orchestra Hit"
    ;;--
    ;; Brass
    ;;++
    "Trumpet"
    "Trombone"
    "Tuba"
    "Muted Trumpet"
    "French Horn"
    "Brass Section"
    "SynthBrass 1"
    "SynthBrass 2"
    ;;--
    ;; Reeds
    ;;++
    "Soprano Sax",              ;; 64
    "Alto Sax"
    "Tenor Sax"
    "Baritone Sax"
    "Oboe"
    "English Horn"
    "Bassoon"
    "Clarinet"
    ;;--
    ;; Pipes
    ;;++
    "Piccolo"
    "Flute"
    "Recorder"
    "Pan Flute"
    "Blown Bottle"
    "Shakuhachi"
    "Whistle"
    "Ocarina"
    ;;--
    ;; Synth Leads
    ;;++
    "Lead 1 (square)"
    "Lead 2 (sawtooth)"
    "Lead 3 (calliope)"
    "Lead 4 (chiff)"
    "Lead 5 (charang)"
    "Lead 6 (voice)"
    "Lead 7 (fifths)"
    "Lead 8 (bass + lead)"
    ;;--
    ;; Synth Pads
    ;;++
    "Pad 1 (new age)"
    "Pad 2 (warm)"
    "Pad 3 (polysynth)"
    "Pad 4 (choir)"
    "Pad 5 (bowed)"
    "Pad 6 (metallic)"
    "Pad 7 (halo)"
    "Pad 8 (sweep)"
    ;;--
    ;; Effects
    ;;++
    "FX 1 (rain)"
    "FX 2 (soundtrack)"
    "FX 3 (crystal)"
    "FX 4 (atmosphere)"
    "FX 5 (brightness)"
    "FX 6 (goblins)"
    "FX 7 (echoes)"
    "FX 8 (sci-fi)"
    ;;--
    ;; Ethnic
    ;;++
    "Sitar"
    "Banjo"
    "Shamisen"
    "Koto"
    "Kalimba"
    "Bag pipe"
    "Fiddle"
    "Shanai"
    ;;--
    ;; Percussion
    ;;++
    "Tinkle Bell"
    "Agogo"
    "Steel Drums"
    "Woodblock"
    "Taiko Drum"
    "Melodic Tom"
    "Synth Drum"
    "Reverse Cymbal"
    ;;--
    ;; Sound Effects
    ;;++
    "Guitar Fret Noise"
    "Breath Noise"
    "Seashore"
    "Bird Tweet"
    "Telephone Ring"
    "Helicopter"
    "Applause"
    "Gunshot"
))

  ;; GM drum notes start at 35 (C), so subtract gm-drum-note-lowest from your
  ;; NOTE number before using this array.
(defconstant gm-drum-note-lowest 35)
  ;; General MIDI drum channel note names.
(defconstant gm-drum-note-names '(
    "Acoustic Bass Drum",       ;; 35, C
    "Bass Drum 1",              ;; 36, C#
    "Side Stick",               ;; 37, D
    "Acoustic Snare",           ;; 38, D#
    "Hand Clap",                ;; 39, E
    "Electric Snare",           ;; 40, F
    "Low Floor Tom",            ;; 41, F#
    "Closed Hi Hat",            ;; 42, G
    "High Floor Tom",           ;; 43, G#
    "Pedal Hi-Hat",             ;; 44, A
    "Low Tom",                  ;; 45, A#
    "Open Hi-Hat",              ;; 46, B
    "Low-Mid Tom",              ;; 47, C
    "Hi Mid Tom",               ;; 48, C#
    "Crash Cymbal 1",           ;; 49, D
    "High Tom",                 ;; 50, D#
    "Ride Cymbal 1",            ;; 51, E
    "Chinese Cymbal",           ;; 52, F
    "Ride Bell",                ;; 53, F#
    "Tambourine",               ;; 54, G
    "Splash Cymbal",            ;; 55, G#
    "Cowbell",                  ;; 56, A
    "Crash Cymbal 2",           ;; 57, A#
    "Vibraslap",                ;; 58, B
    "Ride Cymbal 2",            ;; 59, C
    "Hi Bongo",                 ;; 60, C#
    "Low Bongo",                ;; 61, D
    "Mute Hi Conga",            ;; 62, D#
    "Open Hi Conga",            ;; 63, E
    "Low Conga",                ;; 64, F
    "High Timbale",             ;; 65, F#
    "Low Timbale",              ;; 66, G
    "High Agogo",               ;; 67, G#
    "Low Agogo",                ;; 68, A
    "Cabasa",                   ;; 69, A#
    "Maracas",                  ;; 70, B
    "Short Whistle",            ;; 71, C
    "Long Whistle",             ;; 72, C#
    "Short Guiro",              ;; 73, D
    "Long Guiro",               ;; 74, D#
    "Claves",                   ;; 75, E
    "Hi Wood Block",            ;; 76, F
    "Low Wood Block",           ;; 77, F#
    "Mute Cuica",               ;; 78, G
    "Open Cuica",               ;; 79, G#
    "Mute Triangle",            ;; 80, A
    "Open Triangle"             ;; 81, A#
))
