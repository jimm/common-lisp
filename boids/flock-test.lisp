(load "utils")
(load "structs")
(load "flock")
(load "../test")

(deftest test-flock-init ()
  (let ((flock (cons (make-boid) nil)))
    (check
     (= 1 (length flock))
     (= 0 (point-x (thing-pos (first flock))))
     (= 0 (point-y (thing-pos (first flock))))
     (= 0 (point-z (thing-pos (first flock))))
     )))

(deftest test-flock ()
  (combine-results
   (test-flock-init)))

;; Run tests
(format t "~%Test results: ~:[ERRORS~;OK~]~%" (test-flock))
