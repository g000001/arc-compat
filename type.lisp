(in-package :arc-compat.internal)
(in-readtable :common-lisp)


(defmacro xdeftype (name orig)
  `(deftype ,name (&rest args)
     `(,',orig ,@args)))


(deftype arc:fn (&rest args)
  (cl:if (null args)
         'function
         `(cl:function ,@args)))


(deftype arc:exception () 'cl:condition)

(deftype arc:sym () 'cl:symbol)


(deftype arc:char () 'cl:character)


(xdeftype arc:string cl:string)


(xdeftype arc:int cl:integer)


(deftype arc:num () 'cl:number)


(deftype arc:table () 'cl:hash-table)


(deftype arc:output () 
  `(satisfies cl:output-stream-p))


(deftype arc:input () 
  `(satisfies cl:input-stream-p))


(xdeftype arc:cons
          #-(:and :allegro-version< (:version>= 10 1)) cl:cons
          #+(:and :allegro-version< (:version>= 10 1)) cl:list)


;;; eof

