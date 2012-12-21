(in-package :arc-compat.internal)
(in-readtable :common-lisp)


(defalias =* cl:defvar)


(defalias *let cl:multiple-value-bind)


(def ignore-vars (vars)
  `(cl:declare (cl:ignore ,@vars)))
