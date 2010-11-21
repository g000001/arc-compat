(in-package #:cl-user)

(defpackage #:arc-compat.setagaya.mc
  (:nicknames #:arc)
  (:use #:cl)
  (:shadow 
   #:=
   #:do
   #:assert
   #:++
   #:set
   #:let
   #:nthcdr
   #:adjoin
   #:reduce
   #:rem
   )
  (:export 
   #:AAND #:AFN #:AIF #:AWHEN #:RFN #:TRAV
   #:++ #:-- #:= #:ASSERT #:PULL #:ROTATE #:SCAR #:SCDR #:SET #:WIPE #:ZAP
   #:DO #:FN #:LET #:WITH #:WITHS
   #:NO #:NOR
   #:DRAIN #:REPEAT #:UNTIL #:WHILE #:WHILER #:WHILET
   #:ADJOIN #:CARIF #:CARIS #:CONSIF #:CONSWHEN #:FIRSTN #:FIRSTN-THAT
   #:INTERSPERSE #:JOIN #:LEN #:MAP1 #:MAPPEND #:MEM #:MOST #:N-OF #:NTHCDR
   #:PAIR #:RANGE #:RECLIST #:REDUCE #:REM #:REM #:REV #:RREDUCE #:SPLIT
   #:TRUES #:TUPLES
   #:MAC #:MACEX #:MACEX1 #:UNIQ #:W/UNIQ
   #:IS #:ISA #:TESTIFY
   #:COPY #:DO1 #:IN #:SAFESET)
   )
