;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:asdf)


(defsystem #:arc-compat
  :name "arc-compat"
  :description "Arc compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.9.72"
  :depends-on (:fiveam
               :named-readtables
               #+:it :root.package.it
               :bordeaux-threads
               #+sbcl :sb-introspect
               :ironclad
               :cl-fad)
  :serial T
  :components ((:file "package")
               (:file "type")
	       (:file "utils")
               (:file "ac")
               (:file "base")
               ;; (:file "arc")
	       (:file "boolean")
	       (:file "assignment")
	       (:file "macro")
	       (:file "variables")
	       (:file "anaphoric-op")
               (:file "predicates")
               (:file "io")
               (:file "error")
               ;; 
	       (:file "iteration")
               (:file "math")
               (:file "table")
               (:file "string")
               (:file "list")
               ;; 
               (:file "test-after")
               ;; readtable
               (:file "reader")
               (:file "readtable")
               (:file "readtable-setup")
               (:file "ext")
               (:file "arc.arc")
               (:file "string.arc")
               (:file "pprint.arc")
               (:file "html.arc")
	       ))


(defmethod perform ((o test-op) (c (eql (find-system :arc-compat))))
  (load-system :arc-compat)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :arc-compat.internal
                                                    :arc-compat))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; eof

