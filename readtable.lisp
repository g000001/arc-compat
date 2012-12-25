(in-package :arc-user)


(defreadtable :arc
  (:merge :standard)
  (:macro-char #\[ (fn (srm char)
                     (cl:declare (cl:ignore char))
                     `(fn (arc:_)
                        ,(cl:read-delimited-list #\] srm T))))
  (:syntax-from :standard #\) #\])
  (:dispatch-macro-char #\# #\!
                        (fn (srm char arg)
                          (cl:declare (cl:ignore char arg))
                          (cl:ecase (cl:read-char srm t nil t)
                            ((#\C #\c) 
                             (let cl:*readtable* (find-readtable :standard)
                               (cl:read srm t nil t)))
                            ((#\A #\a) 
                             (let cl:*readtable* (find-readtable :arc)
                               (cl:read srm t nil t))))))
  (:dispatch-macro-char #\# #\[
                        (fn (srm char arg)
                          (cl:declare (cl:ignore char arg))
                          (let key (cl:read srm t nil t)
                            (and (cl:string-equal key :ignore)
                                 (arc-compat.internal::ignore-vars 
                                  (cl:read-delimited-list #\] srm t))))))
  (:case :upcase))


;;; eof
