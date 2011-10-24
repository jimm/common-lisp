(defmacro comment (&rest args))

(comment
(format t "Inside comment.lisp. Shoud NOT see this!")
)
