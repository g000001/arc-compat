(in-package :arc-compat.internal)
(in-readtable :common-lisp)


;;; '(w/uniq)
;;; Copyright 1995 by Paul Graham.
;;; http://lib.store.yahoo.net/lib/paulgraham/onlisp.lisp
;;; http://lib.store.yahoo.net/lib/paulgraham/utx.lisp
;;; http://lib.store.yahoo.net/lib/paulgraham/acl2.lisp
;;;
;;; Documentation String
;;; http://arcfn.com/doc/
;;; Copyright 2008 Ken Shirriff. 

;;; Anaphoric operations

;FIXME
;[code] [Macro] aif expr body [expr body] ...
#|(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: 
 each expr is evaluated until one is true, and then the corresponding body is executed.
 Within the body, the anaphoric variable it refers back to the value of expr."
  `(cl:let ((it ,test-form))
     (if it ,then-form ,else-form)))|#

;>
;(aif (> 1 2) (+ it 1) 42 (+ it 2))
;44

;>(aif nil (+ it 1))
;nil
(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

;[code] [Macro] awhen expr [body ...]
;<- *onlisp*
(defmacro awhen (test-form &body body)
  "Anaphoric when: if the expr is true, the body is executed. 
Within the body, the variable it refers back to the value of expr."
  `(if ,test-form
       (progn ,@body)))

;>(awhen (* 2 3) (+ it 1))
;7

(mac whenlet (var expr &body body)
  `(iflet ,var ,expr (do ,@body)))


(mac unless (test . body)
  `(if (no ,test) (do ,@body)))


;FIXME
;[code] [Macro] afn parms [body ...]
;<- *onlisp* /alambda/
(defmacro afn (parms &body body)
  "Creates an anaphoric function, which can be called recursively
with the name self. This allows a recursive function to be
created without assigning it a name."
  `(labels ((self ,parms ,@body))
     #'self))

;>(funcall (afn (x) (if (is x 0) 1 (* 2 (self (- x 1))))) 5)
;32


(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (cl:null (cdr args)) 
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(funcall ex args))))


(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))


(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))


;[code] [Macro] aand [arg ...]
(defmacro aand (&rest args)
  "Anaphoric and. Returns the last argument if all arguments are
true, otherwise returns nil. Inside each argument the anaphoric
variable it refers to the value of the previous argument. Like
and, lazy evaluation is used, so evaluation stops after
encountering a false argument."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(if ,(car args) (and ,@(cdr args))))))

;>(aand 1 (+ it 2) (* it 10))
;30


;FIXME
;[code] [Macro] rfn name parms [body ...]
(defmacro rfn (name parms &body body)
  "Creates a function with the given name. The name is only inside
the scope of the rfn macro. This allows recursive functions to be
created without polluting the wider scope."
  `(labels ((,name ,parms ,@body))
     (cl:declare (cl:optimize (cl:debug 1)))
     #',name))

;(funcall (rfn pow2 (x) (if (= x 0) 1 (* 2 (pow2 (- x 1))))) 5)
; 32

;[code] [Procedure] trav obj [function ...]
(defmacro trav (x &rest fs)
  "Recursive traversal. Applies each function in sequence to obj,
if obj is not nil. The function can recursively call itself with
a new obj with (self obj)."
  (w/uniq g
    `(labels ((self (,g)
		(when ,g
		  ,@(mapcar (lambda (f) `(,f ,g)) fs))))
       (self ,x))))
  
;(trav '(1 2 3 4) (lambda (_) (print _)) (lambda (_) (self (cdr _))))
;
;(1 2 3 4)
;(2 3 4)
;(3 4)
;(4)

;nil
