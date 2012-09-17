(in-package :cl-user)                   ;cl-user!


(named-readtables:defreadtable :arc
  (:merge :standard)
  (:macro-char #\[ (lambda (srm char)
                     (cl:declare (cl:ignore char))
                     (let ((*readtable* (copy-readtable nil)))
                       `(lambda (arc::_)
                          ,(sb-impl::read-delimited-list #\] srm T)))))
  (:syntax-from :standard #\) #\])
  (:dispatch-macro-char 
   #\# #\!
   (lambda (srm char arg)
     (declare (cl:ignore char arg))
     (ecase (cl:read-char srm t nil t)
       ((#\C #\c) 
        (let ((*readtable*
               (named-readtables:find-readtable :standard)))
          (read srm t nil t)))
       ((#\A #\a) 
        (let ((*readtable* 
               (named-readtables:find-readtable :arc)))
          (read srm t nil t))))))
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


(defun arc-compat.internal::chars->value (chars) 
  (read-from-string (coerce chars 'string)))


(defun arc-compat.internal::symbol->chars (x) 
  (coerce (string x) 'list))


(defun arc-compat.internal::insym? (char sym)
  (member char (arc-compat.internal::symbol->chars sym)))


(defun arc-compat.internal::tokens (test source token acc keepsep?)
  (cond ((null source)
         (reverse (if (consp token) 
                      (cons (reverse token) acc)
                      acc)))
        ((funcall test (car source))
         (arc-compat.internal::tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null token)
                            acc
                            (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (T (arc-compat.internal::tokens test
                   (cdr source)
                   (cons (car source) token)
                   acc
                   keepsep?))))


(defun arc-compat.internal::has-ssyntax-char? (string i)
  (and (>= i 0)
       (or (let ((c (char string i)))
             (or (eql c #\∘) (eql c #\~) ;(eqv? c #\_) 
                 (eql c #\.)  (eql c #\!)))
           (arc-compat.internal::has-ssyntax-char? string (- i 1)))))


(defun arc-compat.internal::ssyntax? (x)
  (and (symbolp x)
       (not (or (eql x 'arc:+) (eql x 'arc:++) (eql x 'arc::_)))
       (let ((name (string x)))
         (arc-compat.internal::has-ssyntax-char? name (- (length name) 1)))))


(defun arc-compat.internal::expand-compose (sym)
  (let ((elts (mapcar (lambda (tok)
                        (if (eql (car tok) #\~)
                         (if (null (cdr tok))
                             'arc:no
                             `(cl:complement 
                               #',(arc-compat.internal::chars->value (cdr tok))))
                         (arc-compat.internal::chars->value tok)))
                      (arc-compat.internal::tokens 
                       (lambda (c) (eql c #\∘))
                       (arc-compat.internal::symbol->chars sym) 
                       '() 
                       '() 
                       'nil))))
    (if (null (cdr elts))
        (car elts)
        (let ((args (gensym)))
          `(lambda (&rest ,args)
             (declare (dynamic-extent ,args))
             (apply (arc::compose ,@(mapcar (lambda (e) `#',e) 
                                            elts))
                    ,args) )))))


(defun arc-compat.internal::expand-ssyntax (sym)
  (funcall 
   (cond ((or (arc-compat.internal::insym? #\∘ sym) 
              (arc-compat.internal::insym? #\~ sym))
          #'arc-compat.internal::expand-compose)
     ;   ((insym? #\_ sym) expand-curry)
         ((or (arc-compat.internal::insym? #\. sym)
              (arc-compat.internal::insym? #\! sym))
          #'arc-compat.internal::expand-sexpr)
         (T (error "~A Unknown ssyntax" sym)))
   sym))


(defun arc-compat.internal::build-sexpr (toks orig)
  (cond ((null toks)
         'arc::get)
        ((null (cdr toks))
         (arc-compat.internal::chars->value (car toks)))
        (T (if (eql (cadr toks) #\!)
               (list 'arc-compat.internal::ref
                     (arc-compat.internal::build-sexpr (cddr toks) orig)
                     (list 'cl:quote
                           (arc-compat.internal::chars->value (car toks))))
               (if (or (eql (car toks) #\.) (eql (car toks) #\!))
                   (error "~A Bad ssyntax" orig)
                   (list 
                    (arc-compat.internal::build-sexpr (cddr toks) orig)
                    (arc-compat.internal::chars->value (car toks))))))))


(defun arc-compat.internal::expand-sexpr (sym)
  (arc-compat.internal::build-sexpr
   (reverse (arc-compat.internal::tokens
             (lambda (c) (or (eql c #\.) (eql c #\!)))
             (arc-compat.internal::symbol->chars sym)
             '()
             '()
             'T))
   sym))


(let ((R (copy-readtable nil)))
  (cl:set-macro-character #\[ 
                          (lambda (SRM CHAR)
                            (cl:declare (cl:ignore CHAR))
                            `(lambda (arc::_)
                               ,(cl:read-delimited-list #\] SRM t) ))
                          nil
                          R )
  (cl:set-syntax-from-char #\] #\) r)
  (defun arc-compat.internal::Read-symbol (STREAM)
    (let* ((*readtable* R)
           (OBJ (read-preserving-whitespace STREAM)) )
      (typecase OBJ
        (symbol (if (arc-compat.internal::Ssyntax? OBJ)
                    (arc-compat.internal::Expand-ssyntax OBJ)
                    OBJ ))
        (otherwise OBJ) ))))


(defun arc-compat.internal::compose-reader-macro-reader (stream char)
  (unread-char char stream)
  (arc-compat.internal::read-symbol stream))


;;; eof
