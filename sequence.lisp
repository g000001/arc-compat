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
        ((or (typep x '(cons (eql the) (cons (cl:member cl:number cl:fixnum))))
             (typep y '(cons (eql the) (cons (cl:member cl:number cl:fixnum)))))
         `(cl:+ ,x ,y))
        (T form)))


(def + obj
  (cl:reduce #'x+y obj))


(define-compiler-macro + (&whole form &rest args)
  (destructuring-bind (x y) args
    (if (cl:= 2 (length args))
        `(x+y ,x ,y)
        form )))


(def recstring (test s (o start 0))
  (let n (len s)
    (funcall
     (afn (i)
       (and (< i (len s))
            (or (funcall test i)
                (self (+ i 1)))))
     start)))


(tst recstring
  (== (let str "abcde"
           (recstring
            (fn (idx) (if (is (cl:char str idx) #\c) (+ 10 idx)))
            str ))
      12 ))

