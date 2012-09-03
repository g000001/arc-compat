(in-package :arc-compat.internal)
(in-readtable :common-lisp)

(in-suite arc-compat)

(def err args
  (cl:error "Error: ~{~A~^ ~}" args))



