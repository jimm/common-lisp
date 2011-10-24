(def batch (csv-file)
  (w/infile in csv-file
    (readline in)			; skip header
    (whiler line (readline in) nil
      (create-mf-program (make-program (tokens line #\,))))))
