;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)

(defpackage #:arc-compat-asd
  (:use #:cl #:asdf)
  (:export :*arc-compat-version-string*))

(in-package #:arc-compat-asd)

(defvar *arc-compat-version-string* "0.0.1"
  "arc-compat's version number as a string.")

(in-package #:cl-user)

(asdf:defsystem #:arc-compat
  :name "arc-compat"
  :description "Arc compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.0.1"
  :serial T
  :components ((:file "package")
	       (:file "utils")
               (:file "base")
	       (:file "macro")
	       (:file "anaphoric-op")
	       (:file "assignment")
	       (:file "boolean")
	       (:file "variables")
	       (:file "iteration")
	       (:file "predicates")
	       (:file "list")
	       )
;	       (:file "y" :depends-on ("package" "x"))
;	       )
  ;:depends-on (#:x)
  )
