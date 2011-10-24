(= amp-names* (table))

;; make a program table
(def make-program (csv-data)
  (let p (table)
    (= (p "id") (gen-guid)
       (p "file-id") (gen-guid)
       (p "amp-id") (csv-data 0)
       (p "title") (csv-data 5)
       (p "public-title") (csv-data 6)
       (p "authors") (csv-data 7)
       (p "description") (csv-data 8)
       (p "clip") (make-clip csv-data))
    p))

(def prog-disk-file-name (prog)
  (string (prog "id") (clip-file-ext (prog "clip"))))

;; create MediaFactory program
(def create-mf-program (program)
  (create-media-files program)
  (prog-create-database-records program)
  (move-orig-file-to-completed-dir clip))

(def create-media-files (prog)
  (let clip (prog "clip")
    (copy-orig-file-to-master-dir clip)
    (process-master-clip clip)
    (create-program-file prog)))

(def create-program-file (prog)
  (ensure-dir (program-dir (prog "amp-id")))
  (with (ext (clip-file-ext (prog "clip"))
	 src (string (module-dir (prog "amp-id")) ((prog "clip") "disk-file-name"))
	 dest (string (program-dir (prog "amp-id")) (prog-disk-file-name prog)))
    (if (is ".mp4" ext) (cp src dest)
	t (do
	    (mp3cat-clip prog src dest)
	    (set-metatags prog dest)))))

(def mp3cat-clip (prog src dest)
  (let tmpfile (string "/tmp/bulk-upload-" (rand-string 8))
    (system:string "echo " (shq src) " >" (shq tmpfile))
    (system:string (script-dir "mp3cat.sh") " " (shq tmpfile) " " (shq dest))))

(def prog-file-size (prog)
  (let dfn (shq (string (program-dir (prog "amp-id")) (prog-disk-file-name prog)))
    (if (is ".mp4" (clip-file-ext c))
	    (system:string "perl " (script-dir) "/mp4-duration.pl " dfn)
	t
	    (system:string (script-dir) "mp3info.sh " dfn))))

(def amplifier-name (amp-id)
  (let amp-name (amp-names* amp-id)
    (when (no amp-name)
      (= (amp-names* amp-id) (execute:string "select concat(first_name, ' ', last_name) from amplifier where amplifier_id = " (q amp-id)))
      (= amp-name (amp-names* amp-id)))
    amp-name))

(def set-metatags (prog dest)
  (withs (amp-id (prog "amp-id")
	  amp-name (amplifier-name amp-id)
	  t (date))
    (system:string (script-dir amp-id) "mp3metatag.sh " (shq dest) " " (prog (clip "title"))
		   " " (shq amp-name) " " (shq:string "iAmplify - " amp-name) " "
		   (cut t 0 4) " " (shq:string "Built on " t) " " (shq "iAmplify Program"))))

(def prog-create-datebase-records (prog)
  (let clip (prog "clip")
    ((prog "clip") create-database-records)
    (execute:string "insert into taggable values " (q (prog "file-id")))
    (with (type-ids (file-type-ids (clip-file-ext clip))
	   file-title (file-name-sans-ext (clip "file-name")))
      (execute:string "insert into file values ("
		      (tostring (prall '(
					 (q (prog "file-id")), 1001, (car type-ids),
					 (q (prog "amp-id")), (q (prog-disk-file-name prog)),
					 (q file-title), (q file-title), 0, "now()",
					 "now()", "null", (q (prog-file-size prog)),
					(q (or (clip "attach-visible") 0))
					(cadr type-ids)
					)))
		     ")"))))
