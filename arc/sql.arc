;; Implements database connectivity by saving SQL into a text file
;; and running that with the mysql shell command.

;; Redefine these values for your database
(= db-default-host* "localhost"
   db-host* "localhost"
   db-default-port* 3306
   db-port* nil				; if nil, uses db-default-port*
   db-user* "username"
   db-pwd* "password"
   db* "database")

(def q (val)
  " Returns val as a quoted SQL string. If val is nil, returns
    \"null\". If val starts with a backquote, returns the value
    minus the backquote with no surrounding quotes or escapes."
  (if (no val) "null"
      (let sval (coerce val 'string)
	(if (is "`" (cut sval 0 1)) (cut sval 1)
		(string "'" (subst "''" "'" sval) "'")))))

(def sql (sql)
  " Executes sql and returns the output stripped of its final newline.
    If the output contains \"ERROR\", it is output to stdout as well."
  (w/uniq g
    (let tf (string "/tmp/" g ".sql")
      (w/outfile f tf (disp (string sql ";") f)) ; write sql to temp file
      (withs (port (or db-port* db-default-port*)
              host (or db-host* db-default-host*)
	      val (tostring:system:string
		    "mysql -h " host " -u " db-user* " -p" db-pwd*
		    " -P " port " --disable-auto-rehash --skip-column-names "
		    db* " <" tf " 2>&1")
	      result (cut val 0 (- (len val) 1)))
	(rmfile tf)
	(if (litmatch "ERROR" result) (err (string sql " --- " result)))
	result))))

(def insert (table vals)
  " Inserts values into table. All values are run through q (which means they
    are quoted unless they are nil or backquoted)."
  (sql:string "insert into " table " values (" (tostring (prall (map [q _] vals))) ")"))
