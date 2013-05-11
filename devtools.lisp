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
                 (rem (fn (_) (or (cl:fboundp _)
                                  (cl:boundp _)))
                      (arc-types)))
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
(PIPE-FROM EXPAND= SWAP DECLARE SSEXPAND SOCKET SPLITN PPR QUASIQUOTE REP
 ENQ-LIMIT ANNOTATE NEW-THREAD TD FILL-TABLE CALL-W/STDOUT
 CURRENT-GC-MILLISECONDS PROTECT READ-TABLE BREAK-THREAD OPEN-SOCKET
 CURRENT-PROCESS-MILLISECONDS SAVE-TABLE LATIN1-HACK CCC LOAD-TABLES SP
 CALL-W/STDIN PEEKC SETFORMS PPREST READB DEFAULT LOAD-TABLE READC FORCE-CLOSE
 SOCKET-ACCEPT THROW KILL-THREAD EXPAND=LIST DEQ ENSURE-DIR MEMORY PPR-PROGN
 PPR-CALL SETTER SREF CLIENT-IP SAFE-LOAD-TABLE READLINE ALREF ENQ EXCEPTION
 CACHE WRITE-TABLE DETAILS SINCE WRITE-SPACED DEAD DEFSET)
379
60

(*arc-version)
940












