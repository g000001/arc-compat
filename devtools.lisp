(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(def arc-compat-external-symbols ()
  (cl:loop :for s :being :each :external-symbol :in :arc :collect s))


(=* arc-keywords* '(index it self _ o))

(def unimplements ()
  (let arcsyms (arc-compat-external-symbols)
    (let unimps (keep (fn (_) (and (not (cl:fboundp _))
                                   (not (cl:boundp _))
                                   (not (mem _ arc-keywords*))))
                      arcsyms)
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
(QUASIQUOTE SETTER SAFEREAD READLINE OPEN-SOCKET CALL-W/STDOUT EXPAND= PEEKC
 BREAK-THREAD PPR-CALL SETFORMS RMFILE READC READFILE1 SINCE FILE-EXISTS
 DIR-EXISTS LOAD-TABLE INPUT BLANK-URL DEFSET SAVE-TABLE TIME10 SREF READB
 WRITE-SPACED READFILE MSEC RPAR SPLITN ONTREE WRITE-TABLE PROTECT CLIENT-IP
 ENQ-LIMIT TD SAFE-LOAD-TABLE ENSURE-DIR FRONT DEQ SECONDS NEW-THREAD CCC PPR
 READALL LATIN1-HACK SOCKET MAPS T! CURRENT-GC-MILLISECONDS NIL! FORCE-CLOSE SP
 LPAR PPREST ANNOTATE SOCKET-ACCEPT INST JTIME REP KILL-THREAD
 CURRENT-PROCESS-MILLISECONDS CACHE PRS PRALL OUTPUT DATE CALL-W/STDIN SSEXPAND
 TEMLOAD DEFTEM SWAP ENQ READ-TABLE DEAD DETAILS LOAD-TABLES END CHAR DEFAULT
 TEMLOADALL THROW MEMORY BOTH TEMPLATIZE TEMREAD PPR-PROGN ALREF W/APPENDFILE
 EXCEPTION DECLARE TREE-SUBST PIPE-FROM EXPAND=LIST MVFILE FILL-TABLE
 WRITEFILE1)
386
97


(*arc-version)




