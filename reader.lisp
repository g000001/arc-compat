(cl:in-package :arc-compat.internal)


(in-readtable :common-lisp)


(defconstant compose-marker #\:)


(defconstant compose-marker2 #\∘)


;;; https://groups.google.com/forum/?hl=ja&fromgroups=#!topic/comp.lang.lisp/kmyEWDT0QGY
;;; kmp
(defun read-tolerant-preserving-whitespace
       (&optional (stream *standard-input*) &rest more-args)
  (flet ((prescan (text)
           ;; Do whatever you want to find and fix package prefixes here.
           ;; This is just an example of a trivial hack that injects a
           ;; backslash in front of any colons it sees.
           (with-output-to-string (scanned-str)
             (cl:let ((backslash nil) (string-quotes nil) (vertical-bars nil))
               (dotimes (i (length text))
                 (cl:let ((ch (cl:char text i)))
                   (cl:cond (backslash (setq backslash nil))
                         ((eql ch #\\)
                          (setq backslash t))
                         ((eql ch #\")
                          (setq string-quotes (not string-quotes)))
                         ((eql ch #\|)
                          (setq vertical-bars (not vertical-bars)))
                         ((eql ch #\:)
                          (write-char #\\ scanned-str)))
                   (write-char ch scanned-str)))))))
       (cl:let ((text-to-read
                 (with-output-to-string (str)
                   (cl:let ((copy-stream (make-echo-stream stream str))
                         (*read-suppress* t))
                     ;; This call to read is only to get the bounds of
                     ;; the expression to scan
                     (apply ;; #'cl:read-preserving-whitespace 
                            #'cl:read-preserving-whitespace 
                            copy-stream
                            more-args)))))
         (apply #'read-from-string (prescan text-to-read) 
                ;; :preserve-whitespace T 
                more-args)))) 


(defun chars->value (chars) 
  (read-from-string (cl:coerce chars 'cl:string))) 


(defun symbol->chars (x) 
  (cl:coerce (cl:string x) 'cl:list)) 


(defun insym? (char sym)
  (member char (symbol->chars sym))) 


(defun ac-tokens (test source token acc keepsep?)
  (cond ((null source)
         (reverse (if (consp token) 
                      (cons (reverse token) acc)
                      acc)))
        ((funcall test (car source))
         (ac-tokens test
                 (cdr source)
                 '()
                 (cl:let ((rec (if (null token)
                                   acc
                                   (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (T (ac-tokens test
                   (cdr source)
                   (cons (car source) token)
                   acc
                   keepsep?)))) 


(defun has-ssyntax-char? (string i)
  (and (>= i 0)
       (or (cl:let ((c (cl:char string i)))
             (or (eql c compose-marker)
                 (eql c compose-marker2)
                 (eql c #\~) ;(eqv? c #\_) 
                 (eql c #\.)  (eql c #\!)))
           (has-ssyntax-char? string (- i 1))))) 


(defun ssyntax? (x)
  (and (symbolp x)
       (not (or (eql x 'arc:+) (eql x 'arc:++) (eql x 'arc::_)))
       (cl:let ((name (cl:string x)))
         (has-ssyntax-char? name (- (length name) 1))))) 


(defun Make-compose-form (ARGS)
  (cl:let ((FS (Recompose-symbols ARGS)))
    (cond ((null (cdr FS))
           (car FS))
          (t `(arc:compose ,@FS))))) 


(defun Recompose-symbols (SYMS)
  (cond ((null SYMS)
         '() )
        ((null (cdr SYMS))
         SYMS)
        ((and (find-package (car SYMS)) (cdr SYMS))
         (cons (intern (cl:string (cadr SYMS)) (car SYMS))
               (Recompose-symbols (cddr SYMS))))
        (T (cons (car SYMS)
                 (Recompose-symbols (cdr SYMS)))))) 


(defun Decompose (FNS ARGS)
  (cond ((null FNS) `((lambda (&rest VALS) (car VALS)) ,@ARGS))
        ((null (cdr FNS)) (cons (car FNS) ARGS))
        (T (list (car FNS) (Decompose (cdr FNS) ARGS))))) 


(defun expand-compose (sym &optional fposp)
  (cl:let ((elts (mapcar (lambda (tok)
                           (if (eql (car tok) #\~)
                               (if (null (cdr tok))
                                   'arc:no
                                   `(cl:complement ,(chars->value (cdr tok))))
                               (chars->value tok)))
                         (ac-tokens 
                          (lambda (c) 
                            (or (eql c compose-marker)
                                (eql c compose-marker2)))
                          (symbol->chars sym) 
                          '() 
                          '() 
                          'nil))))
    (if (null (cdr elts))
        (car elts)
        (cl:let ((fs (Make-compose-form elts)))
          (etypecase fs
            (symbol fs)
            (cons (if fposp
                      `(compose ,@elts)
                      (cl:let ((args (gensym)))
                        `(lambda (&rest ,args)
                           (cl:declare (cl:dynamic-extent ,args))
                           (apply ,(Make-compose-form elts)
                                  ,args)))))))))) 


(defun expand-ssyntax (sym)
  (funcall 
   (cond ((or (insym? compose-marker sym)
              (insym? compose-marker2 sym)
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
   (reverse (ac-tokens
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
              (OBJ (read-tolerant-preserving-whitespace STREAM)))
      (typecase OBJ
        (symbol (cl:if (Ssyntax? OBJ)
                    (Expand-ssyntax OBJ)
                    OBJ))
        (otherwise OBJ))))) 


(defun compose-reader-macro-reader (stream char)
  (unread-char char stream)
  (read-symbol stream)) 


(defun set-arc-lambda (name readfn)
  (setf (cl:get name 'arc-lambda) readfn)
  t) 


;;; *EOF* 


