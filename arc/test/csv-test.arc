; (load "/usr/local/src/Lisp/arc/lib/spec.arc")

(def row (str)
  (w/instring s str (csv-next-row s)))

(= test-csv
  (describe "CSV parser tests"
    (it "Empty file returns nil"
      (no (row "")))
    (it "Newline only returns one empty string"
      (let r (row "\n")
	(and (is 1 (len r))
	     (is "" (car r)))))
    (it "Simple row"
      (let r (row "simple,row,here")
	(and (is 3 (len r))
	     (is "row" (r 1)))))
    (it "Simple row, newline, last col does not have newline"
      (let r (row "simple,row,here\n")
	(and (is 3 (len r))
	     (is "here" (r 2)))))
    (it "Empty strings from empty cols"
      (let r (row ",,\n")
	(and (is 3 (len r))
 	     (is "" (r 0))
	     (is "" (r 1))
	     (is "" (r 2)))))
    (it "Quotes"
      (is 3 (len (row "col 1,col 2,\"col \"\"3\"\"\""))))
    (it "Quotes, newline"
      (is 3 (len (row "col 1,col 2,\"col \"\"3\"\"\"\n"))))
    (it "Commas inside quotes"
      (is 5 (len (row "row one,\"\"\"nasty:,\"\"\",\"has, comma\",\"has \"\"quotes\"\"\",has 'single quotes'"))))
    (it "Commas inside quotes, newline"
      (is 5 (len (row "row one,\"\"\"nasty:,\"\"\",\"has, comma\",\"has \"\"quotes\"\"\",has 'single quotes'\n"))))
    (it "Single quotes don't matter"
      (is 5 (len (row "\"has \"\"comma, in quotes\"\"\",\"has 'comma, in single quotes'\",\"quotes \"\",\"\" surrounding\",,"))))
    (it "Single quotes don't matter, newline"
      (is 5 (len (row "\"has \"\"comma, in quotes\"\"\",\"has 'comma, in single quotes'\",\"quotes \"\",\"\" surrounding\",,\n"))))
    (it "More than one comma inside quoted text"
      (is 5 (len (row "\"odd \"\"number of\"\" double \"\"quotes\",\"two, yes two, commas\",\"two, \"\"commas, second\"\" in quotes\",,"))))
    (it "More than one comma inside quoted text, newline"
      (is 5 (len (row "\"odd \"\"number of\"\" double \"\"quotes\",\"two, yes two, commas\",\"two, \"\"commas, second\"\" in quotes\",,\n"))))
    (it "Unbalanced quotes OK"
      (is 5 (len (row "simple,row,\"internal \"\"quote\",,"))))
    (it "Unbalanced quotes OK, newline"
      (is 5 (len (row "simple,row,\"internal \"\"quote\",,\n"))))
    (it "Newline inside quotes allowed"
      (is 1 (len (row "\"this row continues\non the next line\""))))
    (it "Newline inside quotes allowed, newline"
      (let r (row "\"this row continues\non the next line\"\nextra stuff")
	(and (is 1 (len r))
	     (is "this row continues\non the next line" (r 0)))))
    ))

(print-results (test-csv) t)
