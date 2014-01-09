; The following code computes the number of people who use a  particular shell:

(DEFPACKAGE "REGEXP-TEST" (:use "LISP" "REGEXP"))
(IN-PACKAGE "REGEXP-TEST")
(let ((h (make-hash-table :test #'equal :size 10)) (n 0))
  (with-open-file (f "/etc/passwd")
    (with-loop-split (s f ":")
      (let ((sh (seventh s)))
	(if (gethash sh h)
	    (incf (gethash sh h))
	    (setf (gethash sh h) 1)))))
  (with-hash-table-iterator (i h)
    (loop (multiple-value-bind (r k v) (i)
		(unless r (return))
		(format t "[~d] ~s~30t== ~5:d~%" (incf n) k v)))))

 ; By the way, here's a Clojure version.
(comment
(require ['clojure.string :as 'str])

(let [lines (str/split-lines (slurp "/etc/passwd"))
      line-fields (for [line lines]
                    (str/split line #":"))
      shells (map #(if (> (count %) 6) (nth % 6) nil) line-fields)
      freqs (frequencies shells)]
  (println)
  (doseq [[k v] freqs]
    (printf "%s: %d\n" k v)))
)
