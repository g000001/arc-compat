(in-package :arc)

(defmacro defalias (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (if (macro-function ',orig)
	 (setf (macro-function ',alias) (macro-function ',orig))
	 (setf (symbol-function ',alias) (symbol-function ',orig)))))
