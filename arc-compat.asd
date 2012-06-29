;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)


(asdf:defsystem #:arc-compat
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
	       (:file "list")
	       ))
