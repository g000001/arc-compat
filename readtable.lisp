(in-package :cl-user)                   ;cl-user!


(named-readtables:defreadtable :arc
  (:merge :standard)
  (:macro-char #\[ (lambda (srm char)
                     (cl:declare (cl:ignore char))
                     `(lambda (arc::_)
                        ,(sb-impl::read-delimited-list #\] srm T))))
  (:syntax-from :standard #\) #\])
  #|(:dispatch-macro-char #\# #\"
                        (lambda (srm char arg)
                          (declare (cl:ignore char arg))
                          (let ((args (gensym "ARGS-")))
                           `(cl:lambda (&rest ,args)
                              (declare (dynamic-extent ,args))
                              (cl:apply
                               (arc:compose
                                ,@(mapcar 
                                   (lambda (x) 
                                     (if (and (symbolp x)
                                              (char= (char (symbol-name x) 0)
                                                     #\_))
                                         x
                                         `#',x))
                                   (sb-impl::read-delimited-list #\" srm T)))
                               ,args)))))|#
  #|(:macro-char #\∘ (lambda (srm char)
                     (cl:declare (cl:ignore char))
                     (let ((args (gensym "ARGS-")))
                       `(cl:lambda (&rest ,args)
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


(let ((r (copy-readtable nil)))
  (defun arc-compat.internal::read-symbol (stream)
    (let* ((*readtable* r)
           (obj (read-preserving-whitespace stream)) )
      (typecase obj
        (symbol (let ((symname (ppcre:split #\∘ (symbol-name obj))))
                  (if (cdr symname)
                      (values (mapcar (lambda (s) `#',(intern s)) symname) t)
                      obj )))
        (otherwise obj) ))))


(defun arc-compat.internal::compose-reader-macro-reader (stream char)
  (unread-char char stream)
  (multiple-value-bind (expr win)
                       (arc-compat.internal::read-symbol stream)
    (if win
        (let ((args (gensym)))
          `(lambda (&rest ,args)
             (declare (dynamic-extent ,args))
             (apply (arc::compose ,@expr) ,args) ))
        expr )))


;;; eof
