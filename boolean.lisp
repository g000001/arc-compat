(in-package :arc-compat.internal)
(in-readtable :common-lisp)

;; no
(defalias no cl:not
  "Complement: returns true if expr is false, and false if expr is true.")

;>(no 1)
;nil
;
;>(no nil)
;t

;; and <- cl

;; or <- cl

;; nor [arg ...]
(mac nor (&rest args)
  "The nor boolean operator tests if all of the arguments are
false. It returns t if all arguments are false, and nil if any
arguments are true. It performs 'short-circuit' evaluation, and
doesn't evaluate arguments that follow a true one."
  `(no (cl:or ,@args)))

;>(nor nil nil)
;t

;>(nor nil 1 (/ 1 0))
;nil
