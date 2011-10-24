; to do
; - aliases ("l" for look, "go" or "g" for walk, etc.)

; ================ data ================

(load "wizard-data.lisp")

; ================ game engine ================

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                         `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; ================ game commands ================

(defun look ()
  (append (describe-location *wizard-location* *wizard-nodes*)
          (describe-paths *wizard-location* *wizard-edges*)
          (describe-objects *wizard-location* *wizard-objects* *wizard-object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *wizard-location* *wizard-edges*))
              :key #'cadr)))
    (if next
        (progn (setf *wizard-location* (car next))
               (look))
      '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
                 (objects-at *wizard-location* *wizard-objects* *wizard-object-locations*))
         (push (list object 'body) *wizard-object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *wizard-objects* *wizard-object-locations*)))

(defun reset ()
  (defparameter *wizard-object-locations* '((whiskey living-room)
                                     (bucket living-room)
                                     (chain garden)
                                     (frog garden)))
  (defparameter *wizard-location* 'living-room))

; ================ repl ================

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-eval (sexp)
  (if (member (car sexp) *wizard-allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

; ================ experimentation ================

(load "comment.lisp")

(comment
(inventory)

(pickup 'frog)

(walk 'west)
(look)

(describe-objects 'living-room *wizard-objects* *wizard-object-locations*)
(objects-at 'living-room *wizard-objects* *wizard-object-locations*)

(describe-paths 'living-room *wizard-edges*)
(describe-path '(garden west door))

(describe-location 'garden *wizard-nodes*)
) ; end comment
