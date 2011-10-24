(defstruct point
  (x 0)
  (y 0)
  (z 0))

;; Returns a list containing the x, y, and z values of a point.
(defun point-xyz (p)
  (list (point-x p) (point-y p) (point-z p)))

;; Returns a point make up of the coordinates found in the list xyz.
(defun make-point-xyz (xyz)
  (make-point :x (first xyz) :y (second xyz) :z (third xyz)))

;; Returns t if x, y, and z of two points are the same.
(defun point= (p0 p1)
  (and (= (point-x p0) (point-x p1))
       (= (point-y p0) (point-y p1))
       (= (point-z p0) (point-z p1))))

;; Returns the magintude of point p.
(defun point-magnitude (p)
  (+ (* (point-x p) (point-x p))
     (* (point-y p) (point-y p))
     (* (point-z p) (point-z p))))

;; Normalizes point p, modifying it. Returns p.
(defun point-normalize (p)
  (let ((mag (point-magnitude p)))
    (when (and (not (= 0 mag)) (not (= 1 mag)))
      (setf mag (/ 1.0 (sqrt mag)))
      (setf (point-x p) (* (point-x p) mag))
      (setf (point-y p) (* (point-y p) mag))
      (setf (point-z p) (* (point-z p) mag)))
    p))

;; This macro generates functions that manipulate the x, y, and z
;; of a point based on the x, y, and z of some other point. For
;; example, given (point-manipulate-macro p q +) it will generate
;; (progn (setf (point-x p) (+ (point-x p) (point-x q)))
;;	  (setf (point-y p) (+ (point-y p) (point-y q)))
;;	  (setf (point-z p) (+ (point-z p) (point-z q)))
;; )
(defmacro point-manipulate-macro (p0 p1 operator)
  `(progn (setf (point-x ,p0) (,operator (point-x ,p0) (point-x ,p1)))
	  (setf (point-y ,p0) (,operator (point-y ,p0) (point-y ,p1)))
	  (setf (point-z ,p0) (,operator (point-z ,p0) (point-z ,p1)))))

;; Adds other to p, modifying p. Returns p.
(defun point-add (p other) (point-manipulate-macro p other +) p)

;; Subtracts other from p, modifying p. Returns p.
(defun point-subtract (p other) (point-manipulate-macro p other -) p)

;; Multiplies other and p, modifying p. Returns p.
(defun point-multiply (p other) (point-manipulate-macro p other *) p)

;; Divides p by other, modifying p. Returns p.
(defun point-divide (p other) (point-manipulate-macro p other /) p)

;; Square of distance between two points.
(defun point-square-of-distance-to (p other)
  (let ((dx (- (point-x other) (point-x p)))
	(dy (- (point-y other) (point-y p)))
	(dz (- (point-z other) (point-z p))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

;; Distance between two points.
(defun point-distance-to (p other)
  (sqrt (point-square-of-distance-to p other)))
