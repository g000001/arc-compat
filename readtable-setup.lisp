(cl:in-package :cl-user)

(let ((*readtable* (named-readtables:find-readtable :arc)))
  (map nil 
       (lambda (c)
         (set-macro-character c 
                              #'arc-compat.internal::compose-reader-macro-reader
                              t ))
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ))


;;; eof



#|(let ((cl:*readtable* (named-readtables:find-readtable :arc)))
  (read-from-string "(cl:let())"))|#


(let ((*readtable* (named-readtables:find-readtable :arc)))
  (with-input-from-string (in "((fn (x) x) 8)")
    (cl:read in)))


#|(arc-compat.internal::set-arc-lambda 'fn 
                                     (fn (s c)
                                       #[ignore c]
                                       'cl:lambda))|#


#|(arc-compat.internal::set-arc-lambda 'arc:fn 
                                     nil)|#
;;(cl:get 'arc:fn 'arc-lambda)


#|(arc-compat.internal::set-arc-lambda 
 'rfn 
 (cl:lambda (s &aux (rfn-rest (read-delimited-list #\) s t)))
   ;; (unread-char #\) s)
   `(cl:lambda (&rest args)
      (apply (rfn ,@rfn-rest) args))))|#

#|((rfn (x) x) 8)|#

#|(rfn (x) x)|#

#|((lambda (&rest args)
   (rfn (x) x)))|#


#|(let ((*readtable* (named-readtables:find-readtable :arc)))
  (with-input-from-string (in "((rfn (x) x) 8)")
    (cl:read in)))|#



