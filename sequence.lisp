(in-package :arc-compat.internal)
(in-suite arc-compat)


(def x+y (x y)
  (etypecase x
    (cl:number (cl:+ x y))
    (cl:list (cl:append x y))
    (cl:string (cl:concatenate 'cl:string x y))))


(define-compiler-macro x+y (&whole form x y)
  (cond ((or (numberp x) (numberp y))
         `(cl:+ ,x ,y))
        ((or (stringp x) (stringp y))
         `(cl:concatenate 'cl:string ,x ,y))
        ((or (typep x '(cons (eql cl:the) (cons (cl:member cl:number cl:fixnum))))
             (typep y '(cons (eql cl:the) (cons (cl:member cl:number cl:fixnum)))))
         `(cl:+ ,x ,y))
        (T form)))


(def + obj
  (cl:reduce #'x+y obj))


(define-compiler-macro + (&whole form &rest args)
  (if (cl:= 2 (length args))
      (destructuring-bind (x y) args
        `(x+y ,x ,y))
      form ))


;;; eof

