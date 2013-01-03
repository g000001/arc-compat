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


;; (xdef sig fn-signatures)
;; (xdef apply (lambda (fn . args)
;; (xdef cons cons)
;; (xdef car (lambda (x)
;; (xdef cdr (lambda (x)
;; (xdef is (lambda args (pairwise ar-is2 args)))


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
;; (xdef len (lambda (x)
;; (xdef annotate ar-tag)
;; (xdef type ar-type)
;; (xdef rep ar-rep)
;; (xdef uniq ar-gensym)
;; (xdef ccc call-with-current-continuation)
;; (xdef infile  open-input-file)
;; (xdef outfile (lambda (f . args) 
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
;; (xdef writec (lambda (c . args) 
;; (xdef writeb (lambda (b . args) 
;; (xdef write (lambda args (printwith write   args)))
;; (xdef disp  (lambda args (printwith display args)))
(defun arc:sread (p eof)
  (cl:let ((expr (cl:read p nil eof)))
    (if (eq eof expr) eof expr)))
;; (xdef coerce 
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


;; (xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
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


;; (xdef scdr (lambda (x val) 
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
