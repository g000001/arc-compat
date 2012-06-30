;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:asdf)


(defsystem #:arc-compat
  :name "arc-compat"
  :description "Arc compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.0.4"
  :depends-on (:fiveam)
  :serial T
  :components ((:file "package")
	       (:file "utils")
               (:file "base")
               (:file "table")
	       (:file "macro")
               (:file "predicates")
	       (:file "anaphoric-op")
	       (:file "assignment")
	       (:file "boolean")
	       (:file "variables")
	       (:file "iteration")
               (:file "sequence")
	       (:file "list")
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
