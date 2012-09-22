(in-package :arc-compat.internal)
(in-readtable :common-lisp)


(defmacro defalias (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:if (macro-function ',orig)
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
        ((cl:stringp x)        'arc:string)
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


(defun reduce-&optional (args)
  (cl:let ((count (cl:count 'cl:&optional args)))
    (cl:if (cl:>= 1 count)
           args
           (cl:remove 'cl:&optional args :from-end T :count (1- count)))))


(defun arc-ll-to-cl-ll (list)
  (reduce-&optional 
   (mapcan (lambda (x)
             (cl:if (cl:and (consp x)
                            (eq 'o (car x)) )
                    (cl:list 'cl:&optional (cdr x))
                    (cl:list x) ))
           (to-proper-lambda-list list) )))


(defun  +internal-flatten (tree)
  (cond ((cl:atom tree) tree)
        ((not (cl:listp (cdr tree)))
         (+internal-flatten (list (car tree) (cdr tree))))
        ((cl:listp (car tree))
         (cl:append (+internal-flatten (car tree))
                 (+internal-flatten (cdr tree))))
        (T (cl:cons (car tree)
                    (+internal-flatten (cdr tree))))))


;;; fiveam
(defmacro tst (name &body body)
  `(5am:test ,name ,@body))

(defmacro == (x y)
  `(5am:is (cl:equal ,x ,y)))


(defmacro >_< (x y)
  `(5am:signals ,x ,y))


(defun ref (seq index)
  (etypecase seq
    (cl:hash-table (gethash index seq))
    (cl:sequence (elt seq index))))


(defun (setf ref) (val seq index)
  (etypecase seq
    (cl:hash-table (setf (gethash index seq) val))
    (cl:sequence (setf (elt seq index) val))))


(defmacro w/obcall ((&rest seqs) &body body)
  (cl:let ((idx (gensym)))
    `(macrolet (,@(mapcar (lambda (s)
                            `(,s (,idx)
                                 `(ref ,',s ,,idx)))
                    seqs))
       ,@body)))

(defun caar (xs) (car (car xs)))
(defun cadr (xs) (car (cdr xs)))
(defun cddr (xs) (cdr (cdr xs)))
(defun list (&rest args) (cl:copy-list args))

