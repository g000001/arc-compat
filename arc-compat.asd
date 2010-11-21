;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package #:cl-user)

(defpackage #:arc-compat-asd
  (:use #:cl #:asdf))

(in-package #:arc-compat-asd)

(defvar *arc-compat-version-string* "0.0.1"
  "arc-compat's version number as a string.")
(export '*arc-compat-version-string*)


(in-package #:cl-user)

(asdf:defsystem #:arc-compat
  :name "arc-compat"
  :description "Arc compatible package for CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :maintainer "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :version "0.0.1"
  :components ((:file "package")
	       (:file "utils")
	       (:file "base")
	       (:file "macro" :depends-on ("utils"))
	       (:file "anaphoric-op" :depends-on ("base" "macro"))
	       (:file "assignment" :depends-on ("base" "macro"))
	       (:file "boolean" :depends-on ("macro"))
	       (:file "variables" :depends-on ("base" "macro"))
	       (:file "iteration" :depends-on ("base" "macro"))
	       (:file "predicates" :depends-on ("base" "macro"))
	       (:file "list" :depends-on ("base" "macro" "iteration"))
	       )
;	       (:file "y" :depends-on ("package" "x"))
;	       )
  ;:depends-on (#:x)
  )
