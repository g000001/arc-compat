(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)

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


(5am:test to-proper-lambda-list
  (5am:is (equal (to-proper-lambda-list () ) () ))
  (5am:is (equal (to-proper-lambda-list (list 'a 'b 'c) ) (list 'a 'b 'c)))
  (5am:is (equal (to-proper-lambda-list '(a b . c)) '(a b &rest c)))
  (5am:is (equal (to-proper-lambda-list 'rest) '(&rest rest))))


(defun reduce-&optional (args)
  (cl:let ((count (cl:count 'cl:&optional args)))
    (cl:if (cl:>= 1 count)
           args
           (cl:remove 'cl:&optional args :from-end T :count (1- count)))))


(defun arc-ll-to-cl-ll (list)
  (reduce-&optional 
   (mapcan (lambda (x)
             (cl:if (cl:and (consp x)
                            (string= 'o (car x)) )
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


(5am:test setf-ref
  (cl:let ((a (list 1 2 3)))
    (setf (ref a 1) :a)
    (5am:is (equal a (list 1 :a 3))))
  (cl:let ((h (make-hash-table)))
    (setf (ref h 1) :a)
    (5am:is (equal (ref h 1) :a))))


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

(defmacro w/tco (() cl:&body body)
  `(cl:locally 
        #+sbcl (cl:declare (cl:optimize (cl:debug 1)))
        #-sbcl (cl:declare (cl:optimize (cl:debug 1)))
        ,@body))

;;; eof
