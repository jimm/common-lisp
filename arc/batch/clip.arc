;; make a clip table
(def make-clip (csv-data)
  (let c (table)
    (= (c "id") (gen-guid)
       (c "amp-id") (row 0)
       (c "title") (row 1)
       (c "public-title") (row 2)
       (c "file-name") (row 3)
       (c "attach-visible") (row 4)
       (c "disk-file-name") (string (rand 2147483647) "-" (c "file-name")))
    c))

(def clip-file-ext (clip)
  (downcase (file-ext (clip "file-name"))))

(def copy-orig-file-to-master-dir (clip)
  (let amp (clip "amp-id")
    (ensure-dir (master-dir amp))
    (cp (string (source-dir amp) (clip "file-name"))
	(string (master-dir amp) (clip "disk-file-name")))))

(def process-master-clip (clip)
  (let amp (clip "amp-id")
    (ensure-dir (module-dir amp))
    (with (src (shq:string (master-dir amp) (clip "disk-file-name"))
	   dest (shq:string (module-dir amp) (clip "disk-file-name")))
      (if (is ".mp4" (clip-file-ext clip)) (system (string "cp " src " " dest))
	  t (system:string (script-dir) "/resample.sh " src " " dest)))))

(def clip-file-size (clip)
  (with (amp (clip "amp-id")
	 dfn (shq (string (module-dir (clip "amp-id") (clip "disk-file-name"))))
    (if (is ".mp4" (clip-file-ext c))
	    (system:string "perl " (script-dir) "/mp4-duration.pl " dfn)
	t
	    (system:string (script-dir) "mp3info.sh" " " dfn))))

; first is content type id, second is media type id
(def file-type-ids (extension)
  (if (is ".mp4" extension) '(1008, 1001)
      t '(1000 1000)))

(def create-database-records (clip)
  (with (type-ids (file-type-ids (clip-file-ext clip))
	 file-title (file-name-sans-ext (c "file-name")))
    (execute:string "insert into taggable values " (q (c "id")))
    (execute:string "insert into file values ("
		     (tostring (prall '(
					(q (clip "id")) 1001 (car type-ids)
					(q (clip "amp-id")) (q (clip "disk-file-name"))
					(q (clip "file-title")) (q (clip "file-title"))
					1 "now()" "now()" "null" (clip-file-size clip)
					(q (or (clip "attach-visible") 0))
					(cadr type-ids)
					)))
		     ")")))

(def move-orig-file-to-completed-dir (clip)
  (with (cd (completed-dir (clip "amp-id"))
	 fn (clip "file-name"))
    (ensure-dir cd)
    (mv (string (source-dir (clip "amp-id")) fn)
	(string cd fn))))
