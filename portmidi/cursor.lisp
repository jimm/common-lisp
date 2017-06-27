(defpackage #:km-cursor
  ;; TODO #:goto-song #:goto-song-list #:attempt-goto
  (:export #:cursor #:next-song #:prev-song #:next-patch #:prev-patch))

(in-package #:km-cursor)

(defun cursor (km)
  (get 'cursor km))

(defun next-song (km)
  (let ((c (cursor km)))
  ))

(defun prev-song (km)
  )

(defun next-patch (km)
  )

(defun prev-patch (km)
  )
