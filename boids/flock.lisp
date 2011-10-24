;; Flock methods. A flock is a simple list.

(defun flock-move (flock)
  (dolist (boid flock) (boid-move boid (all-but boid flock))))

(defun flock-center (flock)
  (let ((center (make-point))
	(flock-size (length flock)))
    (dolist (boid flock) (point-add center (thing-pos boid)))
    (point-divide center
		  (make-point :x flock-size :y flock-size :z flock-size))
    center))
