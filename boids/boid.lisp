(defstruct boid (thing)
  (max-speed 10))

;; Boid methods
(defmethod boid-move ((boid boid) flock-minus-boid)
  (format t "moving boid")
  (boid-move-towards-flock-center boid flock-minus-boid)
  (boid-avoid-others boid flock-minus-boid)
  (boid-match-others-velocities boid flock-minus-boid)
  (boid-bound-position boid)
  (boid-limit-speed boid))

(defmethod boid-move-towards-flock-center ((boid boid) flock-minus-boid)
  (let ((flock-center (flock-center flock-minus-boid)))
    (point-subtract flock-center (boid-poisition boid))
    (point-divide flock-center 100.0)	; Move 1% of the way towards the center
    (point-add (boid-vector boid) flock-center)))

(defmethod boid-avoid-others ((boid boid) flock-minus-boid)
  (let ((c (make-point)))
    (mapcar #'(lambda (b)
		(let ((other-pos (boid-pos b)))
		  (when (< (point-square-of-distance-to (boid-pos boid)
							other-pos))
		    (point-add c (boid-pos boid))
		    (point-subtract c other-pos))))
	    flock-minus-boid)
    (point-add (boid-vector boid) c)))

(defmethod boid-match-others-velocities ((boid boid) flock-minus-boid)
  (let ((vel (make-point)))
    (mapcar #'(lambda (b) (point-add vel (boid-vec b)))
	    flock-minus-boid)
    (point-divide vec (length flock-minus-boid))
    (point-subtract vec (boid-vec boid))
    (point-divide vec 8)
    (point-add (boid-vector boid) vel)))

(defmacro boid-bound-coord (coord half-dimension)
  `(cond (< (,coord (boid-pos boid)) ,half-dimension)
	  (setf (,coord v) *boid-bounds-limit-pull*)
	  (setf (,coord v) (- *boid-bounds-limit-pull*))))

(defmethod boid-bound-position ((boid boid))
  (let ((v make-point)
	(half-width (/ *world-width* 2))
	(half-height (/ *world-height* 2))
	(half-depth (/ *world-depth* 2)))

    (boid-bound-coord point-x half-width)
    (boid-bound-coord point-y
		      (+ (- half-height)
			 (boid-almost-ground-level boid)
			 *boid-bounds-limit-above-ground-level*))
    (boid-bound-coord point-z half-depth)

    (point-add (boid-vec boid) v)))

(defmethod boid-limit-speed ((boid boid))
  (let ((speed-squared (point-square-of-distance-to (make-point) (boid-vec))))
    (when (> speed-squared (boid-max-speed-squared boid))
      (point-divide (boid-vec boid) (* (sqrt speed-squared) (boid-max-speed))))))
