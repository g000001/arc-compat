(in-package :arc-compat.internal)

(in-suite arc-compat)


(def newstring (length (o char #\Nul))
  (cl:make-string length :initial-element char))
