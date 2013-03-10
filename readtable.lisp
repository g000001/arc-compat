(in-package :arc-user)

(cl:typep '((compose a b) 8) '(cl:cons (cl:cons (cl:eql arc:compose) *) *))

(defreadtable :arc
  (:merge :standard)
  (:macro-char #\( (let read-list (cl:get-macro-character #\( (find-readtable :standard))
                     (fn (srm char)
                       (let expr (cl:funcall read-list srm char)
                         (if (cl:typep expr '(cl:cons 
                                              (cl:cons (cl:eql arc:compose) *)
                                              *))
                             ;; ((compose ...) ...)
                             (arc-compat.internal::decompose (cl:cdar expr)
                                                             (cdr expr))
                             #|(and (cl:typep expr '(cl:cons cl:symbol *))
                                  (or (arc-compat.internal::insym? 
                                       arc-compat.internal::compose-marker 
                                       (car expr))
                                      (arc-compat.internal::insym? 
                                       arc-compat.internal::compose-marker2 
                                       (car expr))))|#
                             #|(do (cl:print expr)
                                 `(,(arc-compat.internal::expand-compose (car expr)
                                                                         :fpos)
                                    ,@(cdr expr)))|#
                             ;; afn
                             (cl:typep expr '(cl:cons 
                                              (cl:cons 
                                               (cl:or
                                                (cl:eql arc:afn)
                                                (cl:eql arc:rfn)
                                                (cl:eql arc:fn))
                                               *)
                                              *))
                             (cons 'cl:funcall expr)
                             :else
                             expr)))))
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
