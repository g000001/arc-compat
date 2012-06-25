(in-package :arc-compat.internal)

;; '(w/uniq)
;; Copyright 1995 by Paul Graham.
;; http://lib.store.yahoo.net/lib/paulgraham/onlisp.lisp
;; http://lib.store.yahoo.net/lib/paulgraham/utx.lisp
;; http://lib.store.yahoo.net/lib/paulgraham/acl2.lisp
;;
;; Documentation String
;; http://arcfn.com/doc/
;; Copyright 2008 Ken Shirriff.

;[code] [Macro] mac name args [body ...]
(defmacro mac (name args &body body)
  "Creates a macro."
  `(defmacro ,name ,(if (consp args)
                        args
                        `(&rest ,args))
     #|(arnesi:with-lisp1 ,@body)|#
     ,@body))

;[code] [Foundation] macex macro
(defalias macex cl:macroexpand
  "Expands a macro.")

;[code] [Foundation] macex1 macro
(defalias macex1 cl:macroexpand-1
  "Expands a macro to one level.")

;[code] [Foundation] uniq
(defalias uniq cl:gensym
  "Generates a unique symbol.")

;; *onlisp*
(defmacro w/uniq (syms &body body)
  "Assigns a unique symbol to each name in names and executes body.
names can either be a single symbol or a list of symbols."
  `(cl:let ,(mapcar (lambda (s)
		   `(,s (uniq)))
                 (if (atom syms) `(,syms) syms))
     ,@body))

;[code] [Foundation] quasiquote arg
;The backquote ` is shorthand for quasiquote, e.g. `(+ 1 2) is the same as (quasiquote (1 2)). Inside quasiquote, the unquote operator will cause the contents to be evaluated instead of quoated. The unquote-splicing operator will cause contents to be evaluated and spliced into the result. , is shorthand for unquote, and ,@ is shorthand for unquote-splicing.

;[code] [Foundation] quote arg
;The single quote ' is shorthand for quote, e.g. 'x is the same as (quote x)
;<- cl
