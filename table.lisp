(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(def table ()
  (cl:make-hash-table :test 'cl:equal))


(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))


(tst w/table
  (== (let ans '()
        (w/table t1
          (= (ref t1 "a") 42) 
          (maphash (fn (k v) (push (list k v) ans)) t1))
        ans)
      (list (list "a" 42))))


(def tablist (h)
  "Returns the table as a list of key/value pairs"
  (accum a (maptable (fn args (a args)) h)))


(tst tablist
  (== (tablist (obj key1 42 key2 "hello"))
      (copy-list '((KEY1 42) (KEY2 "hello")))))


(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (ref h k) v))
         al)
    h))


(tst listtab
  (let x (copy-list '((key1 val1) (key2 val2)))
    (== x (tablist (listtab x)))))


#|(defun read-table (&optional (i (stdin)) eof)
  (let e (read i eof)
    (if (alist e) (listtab e) e)))|#


(def maptable (f tab)
  (cl:maphash f tab)
  nil)


(mac ontable (k v h . body)
  `(maptable (fn (,k ,v) ,@body) ,h))


(mac obj args
  "Creates a table with the specified key/value pairs. (The table can be 
  considered an object with the keys as field names.)"
  (w/uniq g
    `(let ,g (table)
       ,@(map (fn ((k v)) `(setf (gethash ',k ,g) ,v))
              (pair args))
       ,g)))


(tst obj
  (== (accum a (maptable (fn args (a args) )
                         (obj key1 42 key2 "hello")))
      (copy-list '((KEY1 42) (KEY2 "hello"))) ))


;;; eof
