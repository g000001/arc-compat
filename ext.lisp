(in-package :arc-compat.internal)
(in-readtable :common-lisp)


(defalias =* cl:defvar)


(defalias *let cl:multiple-value-bind)


(def ignore-vars (vars)
  `(cl:declare (cl:ignore ,@vars)))


(defmacro string-equal-in (obj &rest choices)
  "This predicate returns true if x is in the choices. Note that
each choice is a separate argument."
  (w/uniq insym
    `(cl:let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(cl:string-equal ,insym ,c))
                     choices)))))

;;; *EOF*
