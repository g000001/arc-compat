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

(defmacro def (name args &body body)
  `(defun ,name (,@(if (consp args)
                       args
                         `(&rest ,args)))
     ,@body))


(defun type (x)
  (cond ;((ar-tagged? x)     (vector-ref x 1))
        ((consp x)          'cons)
        ((null x)          'sym)
        ((symbolp x)        'sym)
        ((functionp x)     'fn)
        ((characterp x)          'char)
        ((stringp x)        'string)
        ((integerp x)       'int)
        ((numberp x)        'num)     ; unsure about this
        ((hash-table-p x)    'table)
        ((output-stream-p x)   'output)
        ((input-stream-p x)    'input)
        ;((tcp-listener? x)  'socket)
        ;((exn? x)           'exception)
        ;((thread? x)        'thread)
        (T                 (error "Type: unknown type ~A" x))))
