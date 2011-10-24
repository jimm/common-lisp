;; shell quote
(def shq (val)
  (string "'" (subst "\\'" "'" val) "'"))

;; The old replace function is not needed because strings.arc (in Anarki) has
;; subst.

;; return file name extension, including dot.
(def file-ext (fname)
  (let toks (tokens fname #\.)
    (if (no toks) nil
	(last toks))))

;; return file name without extension
(def file-name-sans-ext (fname)
  (let toks (tokens fname #\.)
    (if (no toks) nil
	(rev (cdr (rev toks))))))

;; generate GUID
(def gen-guid () (rand-string 36))
