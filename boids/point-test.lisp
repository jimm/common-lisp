(load "point")
(load "../test")

(deftest test-make-point ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= 0 (point-x p))
     (= 0 (point-y p))
     (= 0 (point-z p))
     (= 2 (point-x q))
     (= 0 (point-y q))
     (= 0 (point-z q))
     (= 1 (point-x r))
     (= 2 (point-y r))
     (= 3 (point-z r)))))

(deftest test-point= ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (point= p p)
     (point= p (make-point))
     (point= q q)
     (point= r r)
     (not (point= p q))
     (not (point= p r))
     (not (point= r q)))))

(deftest test-point-equal ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (point= p p)
     (point= p (make-point))
     (point= q q)
     (point= r r)
     (not (point= p q))
     (not (point= p r))
     (not (point= r q)))))

(deftest test-point-magnitude ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= 0 (point-magnitude p))
     (= 4 (point-magnitude q))
     (= 14 (point-magnitude r)))))

(deftest test-point-modifiers ()
  (combine-results
   (test-point-add)
   (test-point-subtract)
   (test-point-multiply)
   (test-point-divide)
   (test-point-normalize)))

(deftest test-point-normalize ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 30 :y 30 :x 15)))
    (check
     (= 0 (point-magnitude (point-normalize p)))
     (= 1 (point-magnitude (point-normalize q)))
     (= 1 (point-magnitude (point-normalize r))))))

(deftest test-point-add ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= 2 (point-x (point-add p q)))
     (= 3 (point-x (point-add r q)))
     (= 5 (point-x (point-add q r))))))

(deftest test-point-subtract ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= -2 (point-x (point-subtract p q)))
     (= -1 (point-x (point-subtract r q)))
     (= 3 (point-x (point-subtract q r))))))

(deftest test-point-multiply ()
  (let ((p (make-point))
	(q (make-point :x 2))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= 0 (point-x (point-multiply p q)))
     (= 2 (point-x (point-multiply r q)))
     (= 4 (point-x (point-multiply q r))))))

(deftest test-point-divide ()
  (let ((p (make-point))
	(q (make-point :x 2 :y 1 :z 1))
	(r (make-point :z 3 :y 2 :x 1)))
    (check
     (= 0 (point-x (point-divide p q)))
     (= 0.5 (point-x (point-divide r q)))
     (= 4 (point-x (point-divide q r))))))

(deftest test-point-distance ()
  (let ((zero (make-point))
	(p (make-point :x 2))
	(q (make-point :x 3 :y 4 :z 5)))
    (check
     (= (sqrt (+ 1 16 25)) (point-distance-to p q))
     (= (sqrt (+ 1 16 25)) (point-distance-to q p))
     (= (sqrt (+ 9 16 25)) (point-distance-to zero q))
     (= (sqrt (+ 9 16 25)) (point-distance-to q zero)))))

(deftest test-point-operation-chaining ()
  (let ((zero (make-point)))
    (check
     (point= (make-point :x 4 :y 5)
	     (point-add (point-add zero (make-point :x 1 :y 2))
			(make-point :x 3 :y 3))))))

(deftest test-point ()
  (combine-results
   (test-make-point)
   (test-point=)
   (test-point-magnitude)
   (test-point-modifiers)
   (test-point-distance)
   (test-point-operation-chaining)))

;; Run tests
(format t "~%Test results: ~:[ERRORS~;OK~]~%" (test-point))
