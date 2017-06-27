(defpackage #:km
  (:use #:portmidi #:common-lisp #:pm)
  (:export #:load-and-run #:run))

(in-package #:km)

(defun patch (name connections &optional start stop)
  )

(defun song (name patches &optional notes)
  (format "song named ~s~n" name)
  )

(defun run (km)
  )

;; FIXME hard-coded data for now
(defun load-and-run (path)
  (run (load path)))
