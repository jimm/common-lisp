(load "utils.lisp")
(load "structs.lisp")

;; Create a flock of boids
(defvar flock)
(setf flock '())
(dotimes (i 10) (setf flock (cons (make-boid) flock)))

;; Move the flock
(flock-move flock)
