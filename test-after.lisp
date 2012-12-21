(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(tst median
  (== (median (list 1 5 3 2 4))
      3 )
  (== (median (list "dog" "cat" "bird"))
      "cat" ))


