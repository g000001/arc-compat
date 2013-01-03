(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(def arc-compat-external-symbols ()
  (cl:loop :for s :being :each :external-symbol :in :arc :collect s))


(def unimplements ()
  (let arcsyms (arc-compat-external-symbols)
    (let unimps (keep (fn (_) (and (not (cl:fboundp _))
                                 (not (cl:boundp _))))
                      arcsyms)
      (cl:values unimps (len arcsyms) (len unimps)))))


(def *arc-version ()
  (*let (_ all unimp) (unimplements)
        _ all
    (- 1000 unimp)))


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
(PPREST EXPAND=LIST PPR-CALL DEAD PRALL TRIM DECLARE CCC MEMO WRITE-SPACED
 RMFILE ENSURE-DIR MVFILE TIME10 SSEXPAND SIG _ CACHE LOAD-TABLES BLANK SREF
 DIR-EXISTS SP PUSHNEW CLIENT-IP WRITEB LPAR DEQ RPAR O TEMLOADALL FILE-EXISTS
 WRITEFILE1 CHAR SETFORMS SAFE-LOAD-TABLE SPLITN LOAD-TABLE READC LATIN1-HACK
 POSMATCH QUASIQUOTE WRITE-TABLE EXPAND= TREE-SUBST PIPE-FROM PRS DEFAULT INST
 MEMORY SINCE REP BEGINS SECONDS DETAILS PROTECT PLURAL INDEX ENQ-LIMIT TEMLOAD
 NEW-THREAD OPEN-SOCKET TEMPLATIZE ANNOTATE MAPS ALREF READLINE ENDMATCH THROW
 SOCKET-ACCEPT KEYS DEFMEMO READFILE SETTER VALS SYSTEM SWAP BREAK-THREAD
 FINDSUBSEQ ENQ KILL-THREAD DEFTEM FILL-TABLE FORCE-CLOSE CALL-W/STDIN JTIME
 DATE READFILE1 T! CURRENT-GC-MILLISECONDS NUM URLDECODE DIR SAVE-TABLE
 LITMATCH READB READALL W/APPENDFILE TEMREAD NIL! ONTREE
 CURRENT-PROCESS-MILLISECONDS CALL-W/STDOUT SAFEREAD TD DEFSET BLANK-URL
 MULTISUBST IT PPR-PROGN READ-TABLE MSEC PPR FLUSHOUT PEEKC)
378
115

(*arc-version)
885
