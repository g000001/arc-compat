(in-package :arc-compat.internal)
(in-suite arc-compat)


(def x+y (x y)
  (etypecase x
    (cl:number (cl:+ x y))
    (cl:list (cl:append x y))
    (cl:string (cl:concatenate 'cl:string x y))))

(def + obj
  (cl:reduce #'x+y obj))


(def reclist (f xs)
  (and xs (or (funcall f xs) (reclist f (cdr xs)))))


(tst reclist
  (== (let ans ()
        (reclist (fn (x) (push x ans) nil) '(a b c))
        ans)
      '((C) (B C) (A B C)))
  (== (reclist (fn (_) (if (is (len _) 2) _)) '(a b c))
      '(B C)))


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

