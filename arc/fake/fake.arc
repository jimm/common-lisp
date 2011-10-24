(aload "schema.arc")

(= data-dir* "arc/data/")

(def fake ()
  (ensure-dir data-dir*)
  (load-data)
  (asv))
