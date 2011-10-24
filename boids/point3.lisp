;;;; Defines point more simply: as a list containing three elements.

;; Returns t if x, y, and z of two points are the same.
(defun point= (p0 p1) (equal p0 p1))

;; Returns the magintude of point p.
(defun point-magnitude (p)
  (+ (* (first p) (first p))
     (* (second p) (second p))
     (* (third p) (third p))))

;; Normalizes point p, modifying it. Returns p.
(defun point-normalize (p)
  (let ((mag (point-magnitude p)))
    (if (and (not (= 0 mag)) (not (= 1 mag)))
	(progn
	  (setf mag (/ 1.0 (sqrt mag)))
	  (list (* (first p) mag) (* (second p) mag) (* (third p) mag)))
      p)))

;; This macro generates functions that manipulate the x, y, and z
;; of a point based on the x, y, and z of some other point. For
;; example, given (point-manipulate-macro p q +) it will generate
;; (progn (setf (first p) (+ (first p) (first q)))
;;	  (setf (second p) (+ (second p) (second q)))
;;	  (setf (third p) (+ (third p) (third q)))
;; )
(defmacro point-manipulate-macro (p0 p1 operator)
  `(progn (setf (first ,p0) (,operator (first ,p0) (first ,p1)))
	  (setf (second ,p0) (,operator (second ,p0) (second ,p1)))
	  (setf (third ,p0) (,operator (third ,p0) (third ,p1)))))

;;;; Should these be functional, returning new points?

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
  (let ((dx (- (first other) (first p)))
	(dy (- (second other) (second p)))
	(dz (- (third other) (third p))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

;; Distance between two points.
(defun point-distance-to (p other)
  (sqrt (point-square-of-distance-to p other)))
