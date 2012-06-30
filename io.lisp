(in-package :arc-compat.internal)
(in-suite arc-compat)


(defalias disp cl:princ)
(defalias writec cl:write-char)


(def pr args
  (map1 #'disp args)
  (car args))


(def prn args
  (do1 (apply #'pr args)
       (writec #\newline)))


