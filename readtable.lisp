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
  #|(:dispatch-macro-char #\# #\"
                        (fn (srm char arg)
                          (declare (cl:ignore char arg))
                          (let ((args (gensym "ARGS-")))
                           `(cl:fn (&rest ,args)
                              (declare (dynamic-extent ,args))
                              (cl:apply
                               (arc:compose
                                ,@(mapcar 
                                   (fn (x) 
                                     (if (and (symbolp x)
                                              (char= (char (symbol-name x) 0)
                                                     #\_))
                                         x
                                         `#',x))
                                   (sb-impl::read-delimited-list #\" srm T)))
                               ,args)))))|#
  #|(:macro-char #\∘ (fn (srm char)
                     (cl:declare (cl:ignore char))
                     (let ((args (gensym "ARGS-")))
                       `(cl:fn (&rest ,args)
                          (declare (dynamic-extent ,args))
                          (cl:apply
                           (arc:compose
                            ,@(cl:loop 
                                 :for fctn := (cl:read-preserving-whitespace srm t nil t)
                                 :then (progn 
                                         (cl:read-char srm t nil t)
                                         (cl:read-preserving-whitespace srm t nil t))
                                 :collect `#',fctn
                                 :while (eql (cl:peek-char nil srm nil nil t)
                                             #\∘)))
                           ,args)))))|#
  ;; (:macro-char #\^ #'tao-read-toga)
  ;; (:macro-char #\. #'read-|.| 'T)
  (:case :upcase))


;;; eof
