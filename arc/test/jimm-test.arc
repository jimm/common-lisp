; (load "/usr/local/src/Lisp/arc/lib/spec.arc")

(= test-shq
  (describe "Shell quote tests"
    (it "Nil returns non-quoted empty string"
      (is "" (shq nil)))
))
(print-results (test-shq) t)

(= test-file-ext
  (describe "File extension extraction tests"
    (it "No extension"
      (no (file-ext "foo")))
    (it "Simple extension"
      (is "bar" (file-ext "foo.bar")))
    (it "Pick last dot"
      (is "bletch" (file-ext "foo.bar.bletch")))
    (it "Dot file with extension"
      (is "d" (file-ext ".emacs.d")))
    (it "Dot file, no extension, nil returned"
      (no (file-ext ".emacs")))
))
(print-results (test-file-ext) t)

(= test-file-name-sans-ext
  (describe "File name without extension"
    (it "No extension"
      (is "foo" (file-name-sans-ext "foo")))
    (it "Simple extension"
      (is "foo" (file-name-sans-ext "foo.bar")))
    (it "No extension, with dot"
      (is "foo" (file-name-sans-ext "foo.")))
))
(print-results (test-file-name-sans-ext) t)
