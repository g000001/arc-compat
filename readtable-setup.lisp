(cl:in-package :cl-user)

(let ((*readtable* (named-readtables:find-readtable :arc)))
  (map nil 
       (lambda (c)
         (set-macro-character c 
                              #'arc-compat.internal::compose-reader-macro-reader
                              t ))
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ))


;;; eof
