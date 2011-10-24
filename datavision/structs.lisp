;; A point is a list of (X Y)
;; A line is a list of (P0 P1)
;; A border-edge is a list of (Style Thickness Number)
;; A border is a list of edges
;; A section is a list of fields
;; A group is a list of sections

;; (defstruct border-edge
;;   style
;;   (thickness 1.0)
;;   (number 1))

(defstruct field
  report
  section
  (visible t)
  id
  bounds
  format
  border
  value)
