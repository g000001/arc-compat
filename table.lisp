(in-package :arc-compat.internal)

(defun table ()
  (cl:make-hash-table :test 'cl:equal))

(def maptable (f tab)
  (cl:maphash f tab)
  nil)

(mac ontable (k v h . body)
  `(maptable (fn (,k ,v) ,@body) ,h))
