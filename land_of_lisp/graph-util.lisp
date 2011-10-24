; ================ data ================

(defparameter *dot-max-label-length* 30)

; ================ dot functions ================

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *dot-max-label-length*)
            (concatenate 'string (subseq s 0 (- *dot-max-label-length* 3)) "...")
          s))
    ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->graphviz (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
                  (funcall thunk))
  (ext:shell (concatenate 'string "open " fname))) ; Mac OS X

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
                  (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->graphviz (fname nodes edges)
  (dot->graphviz fname
                 (lambda () (graph->dot nodes edges))))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda () (graph->dot nodes edges))))

; ================ extensions ================

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph {")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->graphviz (fname nodes edges)
  (dot->graphviz fname
                 (lambda () (ugraph->dot nodes edges))))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda () (ugraph->dot nodes edges))))

; ================ workspace ================

;; (load "wizard-data.lisp")

;; (graph->graphviz "/tmp/dot.gv" *wizard-nodes* *wizard-edges*)

;; (graph->dot *wizard-nodes* *wizard-edges*)
;; (edges->dot *wizard-edges*)
;; (nodes->dot *wizard-nodes*)

;; (dot-name 24)
