;; shell quote
(def shq (val)
  (if (no val) ""
      (string "'" (subst "\\'" "'" val) "'")))

;; The old replace function is not needed because strings.arc (in Anarki) has
;; subst.

;; return file name extension, not including dot.
(def file-ext (fname)
  (let toks (tokens fname #\.)
    (if (< (len toks) 2) nil
	(last toks))))

;; FIXME WRONG
;; return file name without extension
(def file-name-sans-ext (fname)
  (let toks (tokens fname #\.)
    (if (no toks) nil
	(rev (cdr (rev toks))))))


;; generate GUID
(def gen-guid () (rand-string 36))
