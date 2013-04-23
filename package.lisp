(in-package #:cl-user)
;; (g1:delete-package* :arc-compat.setagaya.mc)


(defpackage :arc-compat.setagaya.mc
  (:use)
  (:nicknames #:arc)
  #+:it (:import-from :it :it :self)
  (:import-from :cl
   :expt :truncate :quote :eval :some
   :sqrt :warn :push :abs :mod
   :pop :round :car
   :nil :* :subseq :/ 
   :time :write
   :cdr :- :load :close :t)
  ;; types
  (:export
   :cons :sym :fn :char :string :int :num :table :output :socket :exception
   :thread :input)
  ;; xdefs
  (:export
   :sig :apply :cons :car :cdr :is :err 
   ;; :nil :t
   :+ :- :* :/ :mod :expt :sqrt :>
   :< :len :annotate :type :rep :uniq :ccc :infile :outfile :instring :outstring
   :inside :stdout :stdin :stderr :call-w/stdout :call-w/stdin :readc :readb
   :peekc :writec :writeb :write :disp :sread :coerce :open-socket :socket-accept
   :new-thread :kill-thread :break-thread :sleep :system :pipe-from :table
   :maptable :protect :rand :dir :file-exists :dir-exists :rmfile :mvfile :macex
   :macex1 :eval :on-err :details :scar :scdr :sref :bound :newstring :trunc
   :exact :msec :current-process-milliseconds :current-gc-milliseconds :seconds
   :client-ip :atomic-invoke :dead :flushout :ssyntax :ssexpand :quit :close
   :force-close :memory :declare :timedate :sin :cos :tan :log)
  (:export
   ;; ext
   :=* :*let :w/obcall :ref :leto :witho :withos
   ;; 
   :_
   :assert
   :aand :abs :accum :acons :adjoin :afn :after :aif :alist :all :alphadig :alref
   :and :andf :annotate :apply :assoc :atend :atlet :atom :atomic :atomic-invoke
   :atwith :atwiths :avg :awhen :before :begins :best :bestn :blank :blank-url
   :bound :break-thread :caar :cddr :cache :cadr :call-w/stdin :call-w/stdout :car
   :carif :caris :case :caselet :catch :ccc :cdr :client-ip :close :coerce
   :char
   :commonest :compare :complement :compose :cons :consif :conswhen :copy :count
   :counts :current-gc-milliseconds :current-process-milliseconds :cut
   :date :dead
   :dedup :def :default :defmemo :defset :deftem :deq :details :dir :dir-exists
   :disp :do :do1 :dotted :downcase :drain :each :ellipsize :empty :endmatch :enq
   :enq-limit :ensure-dir :ero :err :errsafe :eval :even :exact
   :expand-metafn-call :expand= :expand=list :expt :file-exists :fill-table :find
   :findsubseq :firstn :firstn-that :flat :fn :for :forlen :fromstring :headmatch
   :get
   :idfn :if :iflet :in :infile :insert-sorted :inside :insort :insortnew 
   :index :inst
   :instring :intersperse :is :isa :isnt :iso :it :join :jtime :keep :keys
   :kill-thread :last :latin1-hack :len :len< :len>
   :let :list :listtab :litmatch :load
   :load-table :load-tables :loop :lpar :mac :macex :macex1 :map :map1 :mappend
   :maps :maptable :max :median :mem :memo :memtable :merge :mergesort :metafn
   :min :mismatch :mod :most :msec :multiple :multisubst :n-of :newstring :nil
   :nil! :no :noisy-each :nonwhite :nor :nthcdr :num :number 
   :o :obj :odd :on :on-err
   :only :ontable :ontree :open-socket :or :orf :outfile :outstring :pair :peekc
   :pipe-from :plural :point :pop :pos :positive :posmatch :ppr :ppr-call
   :ppr-progn :pprest :pr :prall :prn :protect :prs :prf :pull :punc :push :pushnew
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
   :/ :< :<= := :> :>=
   :catch :throw)
  (:export 
   :front :both :end))


(defpackage :arc-user
  (:use :arc :named-readtables)
  (:import-from :cl :in-package))


(defpackage :arc-compat.internal
  (:use :cl :arc :named-readtables)
  (:shadowing-import-from :arc
   :cddr :subst :count :mismatch :assert :- :number :sort :atom :set :complement
   :or :rem :nil :do :cadr :truncate :cons :case :when :last :subseq :sqrt :if
   :get :catch :throw
   :find :some :type :write :++ :>= :max :* :push :sleep :> :list :and :let
   :apply :coerce :car :/ :t :merge :catch :mod :quote :acons :< :+ :expt :nthcdr
   :round :assoc :load :unless :min :abs :map := :time :string :caar :cdr :pop
   :loop :<= :warn :adjoin :read :reduce :close :pushnew :eval :union
   :char :log)
  (:shadowing-import-from :arc
   :subst :sort :assert :last :adjoin :eval :* :rem :union :time :case :get
   :mismatch :set := :count :catch :< :string :sin :+ :t :if :number :cos :find
   :cdr :warn :throw :sleep :map :quote :expt :apply :load :subseq :atom :> :max
   :reduce :abs :cddr :when :complement :acons :or :tan :cons :mod :list :push
   :sqrt :char :let :caar :/ :coerce :log :cadr :round :- :pushnew :assoc :loop
   :pop :do :read :unless :min :nil :some :nthcdr :<= :and :merge :car :++ :write
   :declare :truncate :close :type :>=)
  (:import-from :fiveam :def-suite :in-suite))


(declaim (ftype (function (function sequence) (values sequence &optional))
                arc:sort))


(declaim (ftype (function (cl:sequence) (values cl:sequence &optional))
                arc:rev))


(declaim (ftype (function (cl:function cl:list) (values cl:list &optional))
                arc:map1))


(5am:def-suite arc-compat.internal::arc-compat)

;;; eof


