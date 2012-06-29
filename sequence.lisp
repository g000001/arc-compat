(in-package :arc-compat.internal)

(in-suite arc-compat)

(def x+y (x y)
  (etypecase x
    (cl:number (cl:+ x y))
    (cl:list (cl:append x y))
    (cl:string (cl:concatenate 'cl:string x y))))

(def + obj
  (cl:reduce #'x+y obj))

