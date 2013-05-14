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
  #+excl
  (excl:arglist name)
  #-(or sbcl excl) nil)


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

(defun x+y (x y)
  (etypecase x
    (cl:number (cl:+ x y))
    (cl:list (cl:append x y))
    (cl:string (cl:concatenate 'cl:string x y))))


(define-compiler-macro x+y (&whole form x y)
  (cond ((cl:or (numberp x) (numberp y))
         `(cl:+ ,x ,y))
        ((cl:or (stringp x) (stringp y))
         `(cl:concatenate 'cl:string ,x ,y))
        ((cl:or (typep x 
                    '(cons (eql cl:the) (cons (cl:member cl:number cl:fixnum))))
             (typep y '(cons (eql cl:the) (cons (cl:member cl:number cl:fixnum)))))
         `(cl:+ ,x ,y))
        (T form)))


(defun arc:+ (&rest obj)
  (cl:reduce #'x+y obj))


(define-compiler-macro arc:+ (&whole form &rest args)
  (cl:if (cl:= 2 (length args))
         (destructuring-bind (x y) args
           `(x+y ,x ,y))
         form ))


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


(defun arc:rep (obj)
  ;;--- FIXME
  obj)


(defalias arc:uniq cl:gensym
  "Generates a unique symbol.")


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


(defun arc:call-w/stdout (stream proc)
  (with-open-stream (cl:*standard-output* stream)
    (cl:funcall proc)))


(defun arc:call-w/stdin (stream proc)
  (with-open-stream (cl:*standard-input* stream)
    (cl:funcall proc)))


(defun arc:readc (&optional (stream cl:*standard-input*))
  "Reads a character from the input-port (or default of stdin). 
   Returns nil on end-of-file."
  (cl:read-char stream nil nil nil))


(defun arc:readb (stream)
  "Reads a character from the input-port (or default of stdin). 
   Returns nil on end-of-file."
  (cl:read-byte stream nil nil))


(defun arc:peekc (&optional (stream cl:*standard-output*))
  "Peeks at the next character from the input port, but leaves the character
   for future reads. It uses stdin if the argument is nil. It returns the 
   character, or nil for end-of-file."
  (cl:peek-char nil stream nil nil nil))


(xdef arc:writec cl:write-char)


(defun arc:writeb (int)
  (cl:write-byte int *standard-output*)) 


(defun arc:write (arg &optional (out (stdout)))
  (cl:write arg :stream out)
  nil)


(xdef arc:disp cl:princ)


(defun arc:sread (p eof)
  (cl:let ((expr (cl:read p nil eof)))
    (cl:if (eq eof expr) eof expr)))


(defun arc:coerce (thing type &optional type-opt)
  (typecase thing
    (char (cl:case type
            (int (cl:char-code thing))
            (string (cl:string thing))
            (cl:otherwise (cl:coerce thing type))))
    (sym (coerce (cl:string thing) type))
    (int (cl:case type
           (string (write-to-string thing :base (cl:or type-opt 10.)))
           (char (cl:code-char thing))
           (cl:otherwise (cl:coerce thing type))))
    ((string 0) (cl:case type
                  (cons nil)
                  (cl:otherwise (cl:coerce thing type))))
    (string (cl:case type
              (int (values (parse-integer thing :radix (cl:or type-opt 10.))))
              (sym (intern thing))
              (cl:otherwise (cl:coerce thing type))))
    (cons (cl:case type
            ((string) (apply #'concatenate 'cl:string
                             (mapcar (lambda (y) (coerce y 'string))
                                     thing)))
            (cl:otherwise (cl:error "Can't coerce ~A > ~A" thing type))))
    (t (cl:cond ((cl:and (cl:typep thing (cl:find-symbol "NODE" :stp))
                         (eq type 'string))
                 (funcall (cl:find-symbol "STRING-VALUE" :stp) thing))
                (T (cl:case type
                     (sym (intern (cl:string thing)))
                     (cl:otherwise (cl:coerce thing type))))))))


;; (xdef open-socket  (lambda (num) (tcp-listen num 50 #t))) 


#|(defun open-socket (num)
  (usocket:socket-listen "localhost"
                         num
                         :reuseaddress t
                         :element-type '(unsigned-byte 8)))|#


;; (xdef socket-accept (lambda (s)

#|(defun socket-accept (socket)
  (usocket:socket-accept socket))|#


(defun arc:new-thread (procedure)
  (bt:make-thread procedure :name (string (gensym "arc-thread-"))))


;; (xdef kill-thread kill-thread)

(defun arc:kill-thread (th)
  "Terminates the specified thread immediately."
  (prog1
    (sb-thread:destroy-thread th) 
    ;;--- FIXME
    #+sbcl (sleep .0000000001)))


(defun arc:break-thread (th)
  (bt:interrupt-thread th #'break))


(defun wrapnil (f) 
  (lambda (&rest args)
    (cl:declare (dynamic-extent args))
    (apply f args) 
    'nil))


(xdef sleep cl:sleep)


(flet ((whitecp (c) 
         (member c '(#\Space #\Newline #\Return #\Tab))))
  (defun split-by-whitec (string)
    (cl:cond ((notany #'whitecp string)
              (list string))
             ((every #'whitecp string)
              '())
             (T (cl:let ((pos (position-if #'whitecp string)))
                  (cons (cl:subseq string 0 pos) 
                        (cl:and pos (split-by-whitec (cl:subseq string (1+ pos))))))))))


(defun arc:system (string)
  #+sbcl
  (cl:destructuring-bind (&optional (cmd :no-cmd) . args)
                         (split-by-whitec string)
    (cl:unless (eq :no-cmd cmd)
      (cl:princ 
       (cl:with-output-to-string (out)
         (sb-ext:run-program cmd args :search t :output out))
       (stdout))))
  nil)


(defun arc:pipe-from (cmd)
  (make-string-input-stream
   (cl:with-output-to-string (cl:*standard-output*)
     (system cmd))))


(defun arc:table ()
  (cl:make-hash-table :test 'cl:equal))


(defun arc:maptable (f tab)
  (cl:maphash f tab)
  nil)


(defun arc:protect (during-procedure after-procedure 
                                     &aux (*debugger-hook* nil))
  (unwind-protect (ignore-errors (funcall during-procedure))
    (funcall after-procedure)))


(xdef rand cl:random)


(defun arc:dir (name)                       ;FIXME
  (cl:if (cl:and (probe-file name))
         (remove ""
                 (mapcar (lambda (_)
                           (cl:let ((ns (file-namestring _)))
                             (subseq ns 0 (position #\/ ns :from-end T))))
                         (cl:directory (format nil "~A/*.*" name)))
                 :test #'string=)
         (error "Error: directory-list: could not open \"~A\"" name)))


(defun file-exists (name)
  (and (not (cl-fad:directory-exists-p name))
       (cl-fad:file-exists-p name)))


(defun dir-exists (name)
  (cl-fad:directory-exists-p name))


(defun rmfile (name)
  (cl:delete-file name))


(defun mvfile (old new)
  (cl:rename-file old new))


(xdef macex cl:macroexpand)


(xdef macex1 cl:macroexpand-1)


;; (xdef eval (lambda (e)


(defun arc:on-err (errfn f)
  (cl:multiple-value-bind (ans cond)
                          (cl:ignore-errors (funcall f))
    (cl:or ans (funcall errfn cond))))


(defun arc:details (condition)
  (apply #'format nil
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))


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
(defun msec ()
  (* #+sbcl (sb-posix:time)
     #-sbcl (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))
     1000))


(xdef arc:current-process-milliseconds cl:get-internal-run-time)
;; (xdef current-gc-milliseconds      current-gc-milliseconds)


(defun arc:current-gc-milliseconds ()
  #+sbcl sb-ext:*gc-run-time*
  #-sbcl 0)


(defun seconds ()
  #+sbcl (sb-posix:time)
  #-sbcl (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))


;; (xdef client-ip (lambda (port) 
(defun arc:atomic-invoke (f)
  (bt:with-recursive-lock-held (ar-the-lock)
    (funcall f)))


(defun dead (thread) 
  (not (bt:thread-alive-p thread)))


(defun arc:flushout ()
  (cl:force-output cl:*standard-output*)
  't)


;; (xdef ssyntax (lambda (x) (tnil (ssyntax? x))))
;; (xdef ssexpand (lambda (x)
(xdef arc:quit #+sbcl sb-ext:exit 
      #-(or sbcl excl) cl-user::quit 
      #+excl excl:exit)
;; (xdef close ar-close)
;; (xdef force-close (lambda args


(defun memory ()
  ;;--- FIXME
  #+sbcl (sb-vm:memory-usage)
  #-sbcl 0)

;; (xdef declare (lambda (key val)


(defun ac-niltree (x)
  (cond ((consp x) (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
        ((null x) 'nil)
        (T x)))


(defun arc:timedate (&optional arg)
  (multiple-value-bind (s m h d mo y)
                       (decode-universal-time (cl:or arg (get-universal-time))
                                              0)
    (list s m h d mo y)))


(xdef arc:sin cl:sin)


(xdef arc:cos cl:cos)


(xdef arc:tan cl:tan)


(xdef arc:log cl:log)


;;; eof
