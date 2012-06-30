(in-package :arc-compat.internal)


(defmacro defalias (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (if (macro-function ',orig)
	 (setf (macro-function ',alias) (macro-function ',orig))
	 (setf (symbol-function ',alias) (symbol-function ',orig)))))


#|(defmacro def (name args &body body)
  `(defun ,name (,@(if (consp args)
                       args
                         `(&rest ,args)))
     (arnesi:with-lisp1
       ,@body)))|#

(deftype fn () 'cl:function)
(deftype sym () 'cl:symbol)
(deftype char () 'cl:character)
(deftype string () 'cl:string)
(deftype int () 'cl:integer)
(deftype num () 'cl:number)
(deftype table () 'cl:hash-table)
(deftype output () 
  `(satisfies cl:output-stream-p))
(deftype input () 
  `(satisfies cl:input-stream-p))


(defun type (x)
  (cond ;((ar-tagged? x)     (vector-ref x 1))
        ((cl:consp x)          'cons)
        ((cl:null x)          'sym)
        ((cl:symbolp x)        'sym)
        ((cl:functionp x)     'fn)
        ((cl:characterp x)          'char)
        ((cl:stringp x)        'string)
        ((cl:integerp x)       'int)
        ((cl:numberp x)        'num)     ; unsure about this
        ((cl:hash-table-p x)    'table)
        ((cl:output-stream-p x)   'output)
        ((cl:input-stream-p x)    'input)
        ;((tcp-listener? x)  'socket)
        ;((exn? x)           'exception)
        ;((thread? x)        'thread)
        (T                 (error "Type: unknown type ~A" x))))


(defun to-proper-lambda-list (list)
  (cl:typecase list
    (cl:null () )
    (cl:list (cl:if (cl:tailp () list)
                    list
                    (cl:let ((last (cl:last list)))
                      `(,@(cl:butlast list)
                          ,(car last)
                          cl:&rest
                          ,(cdr last) ))))
    (cl:symbol `(cl:&rest ,list)) ))


(defun arc-ll-to-cl-ll (list)
  (mapcan (lambda (x)
            (if (cl:and (consp x)
                        (eq 'o (car x)) )
                (list 'cl:&optional (cdr x))
                (list x) ))
          (to-proper-lambda-list list) ))


(defun +INTERNAL-FLATTEN (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
         (append (+INTERNAL-FLATTEN (car lis)) (+INTERNAL-FLATTEN (cdr lis))))
        (t (append (list (car lis)) (+INTERNAL-FLATTEN (cdr lis))))))


;;; fiveam
(defmacro tst (name &body body)
  `(5am:test ,name ,@body))

(defmacro == (x y)
  `(5am:is (cl:equal ,x ,y)))

(defmacro =!= (x y)
  `(5am:signals ,x ,y))
