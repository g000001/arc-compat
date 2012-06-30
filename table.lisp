(in-package :arc-compat.internal)

(def table ()
  (cl:make-hash-table :test 'cl:equal))


(def maptable (f tab)
  (cl:maphash f tab)
  nil)


(mac ontable (k v h . body)
  `(maptable (fn (,k ,v) ,@body) ,h))


(mac obj args
  (w/uniq g
    `(let ,g (table)
       ,@(map (fn ((k v)) `(setf (gethash ',k ,g) ,v))
              (pair args))
       ,g)))



