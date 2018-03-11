# pool

- TODO pool-level config params like `:max-size` and `:wait-strategy` with
  values `:return-nil`, `:wait`, `:create-new`.

# db-pool

Builds on top of `pool`.

- TODO fix clash between names. In REPL I can't call `pool:init`.

# cl:dbi

```lisp
(ql:quickload :cl-dbi)
;; (defvar *conn* (dbi:connect :mysql :database-name "foo" ...))
(dbi:with-connection
    (conn :mysql :database-name "candi_development" :username "root")
  (let* ((query (dbi:prepare conn "select email, firstname from users limit ?"))
         (result (dbi:execute query 10)))
    ;; or (dbi:fetch-all result)
    (loop for row = (dbi:fetch result)
       while row
       do (format t "~a~%" row))))
```

If it complains that it can't find the mysql lib, set `DYLD_LIBRARY_PATH` or
make a symbolic link of dylib at `/usr/lib`.
: export DYLD_LIBRARY_PATH`/usr/local/mysql/lib:$DYLD_LIBRARY_PATH
In my case that needs to be `/opt/local/lib/mysql56/mysql`.

`dbi:fetch` returns a plist (`(name value name value...)`).
Fetch values from plists using `(getf plist 'col-symbol)`.
