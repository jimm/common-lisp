;; csv.arc
;; by jimm@io.com
;; Char-separated string parser.
;;
;; Example:
;;
;;   arc> (w/instring s "col 1,col 2,\"col \"\"3\"\"\"\ncol2.1,col2.2" (drain (csv-next-row s)))
;;   (("col 1" "col 2" "col \"3\"") ("col2.1" "col2.2"))
;;   arc> (w/instring s "col 1,col 2,\"col \"\"3\"\"\"\ncol2.1,col2.2" (drain (csv-next-row s #\tab)))
;;   (("col 1" "col 2" "col \"3\"") ("col2.1" "col2.2"))
;;   arc> (w/outfile f "/tmp/foo.csv" (disp "c1,c2,c3\nc1b,c2b,c3b\n" f))
;;   arc> (w/infile f "/tmp/foo.csv" (drain (csv-next-row f)))
;;   (("c1" "c2" "c3") ("c1b" "c2b" "c3b"))
;;   arc> (rmfile "/tmp/foo.csv")

(def csv-next-row (s (o sep #\,))
  " Returns a list of column values from stream `s' that are
    separated by `sep'."
  (if (no (peekc s))
        nil
      (csv-parse-acc s sep '() "")))

(def csv-parse-acc (s sep cols col)
  (aif (readc s)
         (if (is it #\")                ; start of quoted value
               (csv-parse-acc-quoted s sep cols "")
             (is it sep)                ; start of next column
               (csv-parse-acc s sep (cons col cols) "")
             (is it #\newline)          ; end of line
               (rev (cons col cols))
             (csv-parse-acc s sep cols (string col it))) ; normal char
       (rev (cons col cols))))          ; end of file

(def csv-parse-acc-quoted (s sep cols col)
  (aif (readc s)
         (if (is it #\")                ; quote inside quoted value
               (let next-c (peekc s)
                 (if (is next-c #\")    ; second double quote
                       (do
                         (readc s)
                         (csv-parse-acc-quoted s sep cols (string col #\")))
                     (is next-c sep)    ; separator
                       (do
                         (readc s)
                         (csv-parse-acc s sep (cons col cols) "")) ; next col
                     (is next-c #\newline) ; newline
                       (do
                         (readc s)
                         (rev (cons col cols)))
                     (no next-c)        ; end of file
                       (rev (cons col cols))
                     (ero "format error: malformed quoted string; saw " next-c)))
               (csv-parse-acc-quoted s sep cols (string col it))) ; normal char
         (rev (cons col cols))))        ; end of file
