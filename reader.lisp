(cl:in-package :arc-compat.internal)


(defun chars->value (chars) 
  (read-from-string (coerce chars 'cl:string)))


(defun symbol->chars (x) 
  (coerce (cl:string x) 'cl:list))


(defun insym? (char sym)
  (member char (symbol->chars sym)))


(defun tokens (test source token acc keepsep?)
  (cond ((null source)
         (reverse (if (consp token) 
                      (cons (reverse token) acc)
                      acc)))
        ((funcall test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (cl:let ((rec (if (null token)
                                   acc
                                   (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (T (tokens test
                   (cdr source)
                   (cons (car source) token)
                   acc
                   keepsep?))))


(defun has-ssyntax-char? (string i)
  (and (>= i 0)
       (or (cl:let ((c (cl:char string i)))
             (or (eql c #\∘) (eql c #\~) ;(eqv? c #\_) 
                 (eql c #\.)  (eql c #\!)))
           (has-ssyntax-char? string (- i 1)))))


(defun ssyntax? (x)
  (and (symbolp x)
       (not (or (eql x 'arc:+) (eql x 'arc:++) (eql x 'arc::_)))
       (cl:let ((name (cl:string x)))
         (has-ssyntax-char? name (- (length name) 1)))))


(defun Make-compose-form (ARGS)
  (cl:do ((ARGS ARGS)
          (ANS '() ))
       ((endp ARGS) `(arc::compose ,@(nreverse ANS)))
    (cl:let ((PKG (elt ARGS 0))
             (SYM (elt ARGS 1)))
      (cond ((find-package PKG)
             (or (find-symbol (cl:string SYM) PKG)
                 (error "find-symbol ~A ~A => NIL in ~A" 
                        SYM
                        PKG
                        'Make-compose-form))
             (push `#',(intern (cl:string SYM) PKG)
                   ANS)
             (setq ARGS (cddr ARGS)))
            ('t (push `#',PKG ANS)
                (pop ARGS))))))


(defun expand-compose (sym)
  (cl:let ((elts (mapcar (lambda (tok)
                           (if (eql (car tok) #\~)
                               (if (null (cdr tok))
                                   'arc:no
                                   `(cl:complement 
                                     #',(chars->value (cdr tok))))
                               (chars->value tok)))
                         (tokens 
                          (lambda (c) (eql c #\∘))
                          (symbol->chars sym) 
                          '() 
                          '() 
                          'nil))))
    (if (null (cdr elts))
        (car elts)
        (cl:let ((args (gensym)))
          `(lambda (&rest ,args)
             (declare (dynamic-extent ,args))
             (apply ,(Make-compose-form elts)
                    ,args)
             #|(apply (arc::compose ,@(mapcar (lambda (e) `#',e) 
                                            elts))
                    ,args)|# )))))


(defun expand-ssyntax (sym)
  (funcall 
   (cond ((or (insym? #\∘ sym) 
              (insym? #\~ sym))
          #'expand-compose)
     ;   ((insym? #\_ sym) expand-curry)
         ((or (insym? #\. sym)
              (insym? #\! sym))
          #'expand-sexpr)
         (T (error "~A Unknown ssyntax" sym)))
   sym))


(defun build-sexpr (toks orig)
  (cond ((null toks)
         'arc::get)
        ((null (cdr toks))
         (chars->value (car toks)))
        (T (if (eql (cadr toks) #\!)
               (list 'ref
                     (build-sexpr (cddr toks) orig)
                     (list 'cl:quote
                           (chars->value (car toks))))
               (if (or (eql (car toks) #\.) (eql (car toks) #\!))
                   (error "~A Bad ssyntax" orig)
                   (list 
                    (build-sexpr (cddr toks) orig)
                    (chars->value (car toks))))))))


(defun expand-sexpr (sym)
  (build-sexpr
   (reverse (tokens
             (lambda (c) (or (eql c #\.) (eql c #\!)))
             (symbol->chars sym)
             '()
             '()
             'T))
   sym))


(cl:let ((R (copy-readtable nil)))
  (cl:set-macro-character #\[ 
                          (lambda (SRM CHAR)
                            (cl:declare (cl:ignore CHAR))
                            `(lambda (arc::_)
                               ,(cl:read-delimited-list #\] SRM t) ))
                          nil
                          R )
  (cl:set-syntax-from-char #\] #\) r)
  (defun Read-symbol (STREAM)
    (cl:let* ((*readtable* R)
           (OBJ (read-preserving-whitespace STREAM)) )
      (typecase OBJ
        (symbol (if (Ssyntax? OBJ)
                    (Expand-ssyntax OBJ)
                    OBJ ))
        (otherwise OBJ) ))))


(defun compose-reader-macro-reader (stream char)
  (unread-char char stream)
  (read-symbol stream))


;;; eof
