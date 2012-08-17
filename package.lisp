(in-package #:cl-user)
;; (g1:delete-package* :arc-compat.setagaya.mc)
(defpackage :arc-compat.setagaya.mc
  (:nicknames #:arc)
  (:import-from :it :it :self)
  (:import-from :cl
   :expt :truncate :quote :eval :some
   :cddr :sqrt :warn :push :abs :mod
   :pop :atom :list :sleep :round :car :or
   :nil :* :subseq :cons :subst :catch :/ 
   :assoc :complement :time :write
   :apply :caar :cdr :- :load :close :t
   :cadr)
  (:export
   :assert
   :aand :abs :accum :acons :adjoin :afn :after :aif :alist :all :alphadig :alref
   :and :andf :annotate :apply :assoc :atend :atlet :atom :atomic :atomic-invoke
   :atwith :atwiths :avg :awhen :before :begins :best :bestn :blank :blank-url
   :bound :break-thread :caar :cache :cadr :call-w/stdin :call-w/stdout :car
   :carif :caris :case :caselet :catch :ccc :cddr :cdr :client-ip :close :coerce
   :commonest :compare :complement :compose :cons :consif :conswhen :copy :count
   :counts :current-gc-milliseconds :current-process-milliseconds :cut
   :date :dead
   :dedup :def :default :defmemo :defset :deftem :deq :details :dir :dir-exists
   :disp :do :do1 :dotted :downcase :drain :each :ellipsize :empty :endmatch :enq
   :enq-limit :ensure-dir :ero :err :errsafe :eval :even :exact
   :expand-metafn-call :expand= :expand=list :expt :file-exists :fill-table :find
   :findsubseq :firstn :firstn-that :flat :fn :for :forlen :fromstring :headmatch
   :idfn :if :iflet :in :infile :insert-sorted :inside :insort :insortnew :inst
   :instring :intersperse :is :isa :isnt :iso :it :join :jtime :keep :keys
   :kill-thread :last :latin1-hack :len :len< :len>
   :let :list :listtab :litmatch :load
   :load-table :load-tables :loop :lpar :mac :macex :macex1 :map :map1 :mappend
   :maps :maptable :max :median :mem :memo :memtable :merge :mergesort :metafn
   :min :mismatch :mod :most :msec :multiple :multisubst :n-of :newstring :nil
   :nil! :no :noisy-each :nonwhite :nor :nthcdr :num :number :obj :odd :on :on-err
   :only :ontable :ontree :open-socket :or :orf :outfile :outstring :pair :peekc
   :pipe-from :plural :point :pop :pos :positive :posmatch :ppr :ppr-call
   :ppr-progn :pprest :pr :prall :prn :protect :prs :pull :punc :push :pushnew
   :qlen :qlist :quasiquote :queue :quit :quote :rand :rand-choice :rand-string
   :random-elt :range :read :read-table :readall :readb :readc :readfile
   :readfile1 :readline :readstring1 :reclist :recstring :reduce :reinsert-sorted
   :rem :rep :repeat :rev :rfn :rmfile :rotate :round :roundup :rpar :rreduce
   :safe-load-table :saferead :safeset :save-table :scar :scdr :seconds :set
   :self
   :setforms :setter :sig :since :single :sleep :socket-accept :some :sort :sp
   :split :splitn :sqrt :sread :sref :ssexpand :ssyntax :stderr :stdin :stdout
   :string :subseq :subst :summing :swap :sym :system :t :t! :table :tablist :td
   :temload :temloadall :templatize :temread :testify :thread :time :time10
   :to-nearest :tokens :tostring :trav :tree-subst :trim :trues :trunc
   :truncate :tuples
   :type :uniq :unless :until :upcase :urldecode :vals :w/appendfile :w/infile
   :union
   :w/instring :w/outfile :w/outstring :w/stdin :w/stdout :w/table :w/uniq :warn
   :when :whenlet :while :whiler :whilet :whitec :with :withs :write
   :write-spaced :write-table :writeb :writec :writefile1 :zap :* :+ :++ :- :--
   :/ :< :<= := :> :>=))

(defpackage :arc-user
  (:use :arc)
  (:import-from :cl :in-package))


(defpackage :arc-compat.internal
  (:use :cl :arc)
  (:shadowing-import-from :arc
   :cddr :subst :count :mismatch :assert :- :number :sort :atom :set :complement
   :or :rem :nil :do :cadr :truncate :cons :case :when :last :subseq :sqrt :if
   :find :some :type :write :++ :>= :max :* :push :sleep :> :list :and :let
   :apply :coerce :car :/ :t :merge :catch :mod :quote :acons :< :+ :expt :nthcdr
   :round :assoc :load :unless :min :abs :map := :time :string :caar :cdr :pop
   :loop :<= :warn :adjoin :read :reduce :close :pushnew :eval :union)
  (:shadow :char)
  (:import-from :fiveam :def-suite :in-suite))

(declaim (ftype (function (function sequence) (values sequence &optional))
                arc:sort))

(declaim (ftype (function (cl:sequence) (values cl:sequence &optional))
                arc:rev))

(declaim (ftype (function (cl:function cl:list) (values cl:list &optional))
                arc:map1))
;;; 


