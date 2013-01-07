(in-package :arc-compat.internal)
(in-readtable :standard)
(in-suite arc-compat)


(defvar ar-the-lock (bt:make-lock "Arc"))


(defmacro xdef (alias orig &optional doc-string)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:if (macro-function ',orig)
            (setf (macro-function ',alias) (macro-function ',orig))
            (setf (symbol-function ',alias) (symbol-function ',orig)))))


(defun arc:sig (name)
  #+sbcl
  (sb-introspect:function-lambda-list name)
  #-sbcl nil)


(xdef arc:apply cl:apply)


(xdef arc:cons cl:cons)


;; (xdef car (lambda (x)
;; (xdef cdr (lambda (x)


(defun arc:is (val &rest vals)
  "Tests equality with eqv?"
  (cl:flet ((eqfn (x y)
	      (cl:or (cl:eql x y) (cl:and (cl:stringp x)
				    (cl:stringp y)
				    (cl:string= x y)))))
    (cl:every (lambda (_) (eqfn val _))
	      vals)))


(defun arc:err (&rest args)
  (cl:error "Error: ~{~A~^ ~}" args))


;; (xdef nil 'nil)
;; (xdef t   't)
;; (xdef + (lambda args
;; (xdef - -)
;; (xdef * *)
;; (xdef / /)
;; (xdef mod modulo)
;; (xdef expt expt)
;; (xdef sqrt sqrt)
;; (xdef > (lambda args (pairwise ar->2 args)))
;; (xdef < (lambda args (pairwise ar-<2 args)))


(defun arc:len (seq)
  "Computes the length of seq."
  (etypecase seq
    (cl:sequence (cl:length seq))
    (arc:table (cl:hash-table-count seq))))


;; (xdef annotate ar-tag)


(defun arc:type (x)
  (cl:etypecase x
    (arc:cons   'arc:cons)
    (cl:null    'arc:sym)
    (arc:sym    'arc:sym)
    (arc:fn     'arc:fn)
    (arc:char   'arc:char)
    (arc:string 'arc:string)
    (arc:int    'arc:int)
    (arc:num    'arc:num)
    (arc:table  'arc:table)
    (arc:output 'arc:output)
    ;((tcp-listener? x)  'socket)
    ;;((exn? x)           'exception)
    ;;((thread? x)        'thread)
    ))


;; (xdef rep ar-rep)
;; (xdef uniq ar-gensym)
;; (xdef ccc call-with-current-continuation)


(defun arc:infile (name)
  (cl:open name :direction :input))


(defun arc:outfile (name)
  (cl:open name :direction :output
           :if-exists :supersede
           :if-does-not-exist :create))


(defun arc:instring (string)
  (cl:make-string-input-stream string))


(defun arc:outstring ()
  (cl:make-string-output-stream))


(xdef inside cl:get-output-stream-string)


(defun arc:stdout ()
  cl:*standard-output*)


(defun arc:stdin ()
  cl:*standard-input*)


(defun arc:stderr ()
  cl:*error-output*)


;; (xdef call-w/stdout
;; (xdef call-w/stdin
;; (xdef readc (lambda (str) 
;; (xdef readb (lambda (str)
;; (xdef peekc (lambda (str) 


(xdef arc:writec cl:write-char)


;; (xdef writeb (lambda (b . args) 
;; (xdef write (lambda args (printwith write   args)))


(xdef arc:disp cl:princ)


(defun arc:sread (p eof)
  (cl:let ((expr (cl:read p nil eof)))
    (if (eq eof expr) eof expr)))


(defun coerce (thing type)
  (typecase thing
    (char (cl:case type
            (string (cl:string thing))
            (cl:otherwise (cl:coerce thing type))))
    (sym (coerce (cl:string thing) type))
    (int (cl:case type
           (string (write-to-string thing))
           (cl:otherwise (cl:coerce thing type))))
    ((string 0) (cl:case type
                  (cons nil)
                  (cl:otherwise (cl:coerce thing type))))
    (t (cl:case type
         (sym (intern (cl:string thing)))
         (cl:otherwise (cl:coerce thing type))))))


;; (xdef open-socket  (lambda (num) (tcp-listen num 50 #t))) 
;; (xdef socket-accept (lambda (s)
;; (xdef new-thread thread)
;; (xdef kill-thread kill-thread)
;; (xdef break-thread break-thread)
;; (xdef sleep (wrapnil sleep))
;; (xdef system (wrapnil system))
;; (xdef pipe-from (lambda (cmd)


(defun arc:table ()
  (cl:make-hash-table :test 'cl:equal))


(defun arc:maptable (f tab)
  (cl:maphash f tab)
  nil)


;; (xdef protect protect)
;; (xdef rand random)
;; (xdef dir (lambda (name)
;; (xdef file-exists (lambda (name)
;; (xdef dir-exists (lambda (name)
;; (xdef rmfile (wrapnil delete-file))
;; (xdef mvfile (lambda (old new)
;; (xdef macex (lambda (e) (ac-macex (ac-denil e))))
;; (xdef macex1 (lambda (e) (ac-macex (ac-denil e) 'once)))
;; (xdef eval (lambda (e)
(defun arc:on-err (errfn f)
  (cl:multiple-value-bind (ans cond)
                          (cl:ignore-errors (funcall f))
    (cl:or ans (funcall errfn cond))))


;; (xdef details (lambda (c)


(defun arc:scar (list expr)
  "Sets car of list to a new expression. If applied to a string,
   sets the first character of the string, which must have length at
   least one."
  (etypecase list
    (cl:list (setf (cl:car list) expr))
    (cl:string (setf (cl:char list 0) expr))))


(defun arc:scdr (list val)
  "Sets cdr of a list."
  (cl:rplacd list val)
  nil)


;; (xdef sref 
;; (xdef bound (lambda (x) (tnil (bound? x))))
(defun arc:newstring (length &optional (char #\Nul))
  "Creates a string of the given length."
  (cl:make-string length :initial-element char))


(defun arc:trunc (num)
  "Truncates to an integer. Was 'truncate' in arc0."
  (cl:values (cl:truncate num)))
;; (xdef exact (lambda (x) 
;; (xdef msec                         current-milliseconds)
;; (xdef current-process-milliseconds current-process-milliseconds)
;; (xdef current-gc-milliseconds      current-gc-milliseconds)
;; (xdef seconds current-seconds)
;; (xdef client-ip (lambda (port) 
(defun arc:atomic-invoke (f)
  (bt:with-recursive-lock-held (ar-the-lock)
    (funcall f)))


;; (xdef dead (lambda (x) (tnil (thread-dead? x))))


(defun arc:flushout ()
  (cl:force-output cl:*standard-output*)
  't)


;; (xdef ssyntax (lambda (x) (tnil (ssyntax? x))))
;; (xdef ssexpand (lambda (x)
(xdef arc:quit #+sbcl sb-ext:exit #-sbcl cl-user::quit)
;; (xdef close ar-close)
;; (xdef force-close (lambda args
;; (xdef memory current-memory-use)
;; (xdef declare (lambda (key val)

(defun arc:timedate (&optional arg)
  (multiple-value-bind (s m h d mo y)
                       (decode-universal-time (or arg (get-universal-time))
                                              0)
    (list s m h d mo y)))


(xdef arc:sin cl:sin)


(xdef arc:cos cl:cos)


(xdef arc:tan cl:tan)


(xdef arc:log cl:log)


;;; eof
