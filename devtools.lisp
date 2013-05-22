(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(def arc-compat-external-symbols ()
  (cl:loop :for s :being :each :external-symbol :in :arc :collect s))


(=* arc-keywords* '(index it self _ o))

(def arc-types ()
  (keep #'sb-ext:valid-type-specifier-p
        (arc-compat-external-symbols)))


(def unimplements ()
  (let arcsyms (arc-compat-external-symbols)
    (let unimps (set-difference
                 (keep (fn (_) (and (not (cl:fboundp _))
                                    (not (cl:boundp _))
                                    (not (mem _ arc-keywords*))))
                       arcsyms)
                 (cl:list* 'throw
                           'quasiquote
                           'latin1-hack
                           'annotate
                           'default
                           'declare
                           (rem (fn (_) (or (cl:fboundp _)
                                            (cl:boundp _)))
                                (arc-types))))
         (cl:values unimps (len arcsyms) (len unimps)))))


(def *arc-version ()
  (*let (_ all unimp) (unimplements)
        _ all
    (- 1000 unimp)))


(def cl-arc ()
  (flet ((exports (pkg)
           (cl:loop :for s :being :each :external-symbol :in pkg
              :collect s)))
    (cl:intersection (exports :arc)
                     (exports :cl)
                     :test #'string=)))


;; (map #'kl:ensure-keyword (cl-arc))


#|(let ((cl:*readtable* (named-readtables:find-readtable :arc)))
  (read-from-string "(cl:let())"))|#


#|(let ((*readtable* (named-readtables:find-readtable :arc)))
  (with-input-from-string (in "((fn (x) x) 8)")
    (cl:read in)))|#


#|(arc-compat.internal::set-arc-lambda 'fn 
                                     (fn (s c)
                                       #[ignore c]
                                       'cl:lambda))|#


#|(arc-compat.internal::set-arc-lambda 'arc:fn 
                                     nil)|#
;;(cl:get 'arc:fn 'arc-lambda)


#|(arc-compat.internal::set-arc-lambda 
 'rfn 
 (cl:lambda (s &aux (rfn-rest (read-delimited-list #\) s t)))
   ;; (unread-char #\) s)
   `(cl:lambda (&rest args)
      (apply (rfn ,@rfn-rest) args))))|#

#|((rfn (x) x) 8)|#

#|(rfn (x) x)|#

#|((lambda (&rest args)
   (rfn (x) x)))|#


#|(let ((*readtable* (named-readtables:find-readtable :arc)))
  (with-input-from-string (in "((rfn (x) x) 8)")
    (cl:read in)))|#


(unimplements)
(EXPAND=LIST SETFORMS CLIENT-IP SOCKET-ACCEPT SOCKET SSEXPAND OPEN-SOCKET
 EXPAND=)
377
8

(*arc-version)
990



