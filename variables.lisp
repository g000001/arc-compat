(in-package :arc-compat.internal)

;;; Variables

;; do1 expr [body ...]
(defalias do1 cl:prog1
  "Saves the first expression and returns it after executing the body.")

;>(let x 42
;  (do1 x (= x 50)))
;42

;; def name params [body ...]

;;[code] [Macro] defmemo name parms [body ...]
;;Creates a memoized function.
	
;;[code] [Procedure] memo f
;;Creates a memoized function from f.
	
;; safeset var value
(defmacro safeset (var val)
  "Sets var to value. This is similar to =, except it prints a
warning if var is already defined."
  `(progn
     (when (boundp ',var)
       (warn "*** redefining ~S" ',var))
     (cl:setq ,var ,val)))

;>(do (safeset var 1) (safeset var 2))
;*** redefining var

;2

;; in x [choices ...]
;; <- *onlisp*
(defmacro in (obj &rest choices)
  "This predicate returns true if x is in the choices. Note that
each choice is a separate argument."
  (w/uniq insym
    `(cl:let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

;>(in 1 2 "b" 1 #\c)
;t

;>(in 1 '(1 2 3))
;nil

;; FIXME
;; copy x [key val] ...
(defmacro copy (x &rest key-and-val)
  "Copies x, updating entries corresponding to the args.. The input
must be of type sym, cons, string, or table. For a sym,
additional arguments are not permitted. For a list, the keys are
numeric indices into the list, and the corresponding entries are
replaced with the values. For a string, the keys are indices into
the string and the values must be characters. For a table, the
keys and values add or update entries in the table. Original
object x is unmodified."
  (w/uniq new
    `(cl:let ((,new (etypecase ,x (sequence (copy-seq ,x)))))
       (if ',key-and-val
	   (progn ,@(cl:loop
                       :for kv :on key-and-val :by #'cddr
                       :collect `(setf (elt ,new ,(car kv)) ,(cadr kv)))
		  ,new)
	   ,new))))

;(coerce 'foo 'string)
;(copy "foo" 0 #\2)
;(copy '(foo bar baz) 1 #\2 0 8 )

;[code] [Variable] sig
;Hash table containing function signatures.

;>(sig 'map)
;(f . seqs)

;[code] [Macro] defhook name [body ...]
;Creates a function (similar to def), except the function is registered in hooks*
	
;[code] [Procedure] hook name [arg ...]
;Executes a function registered in hooks*
	
;[code] [Variable] hooks*
;(tests)
