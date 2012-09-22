(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)

;;; This software is copyright (c) Paul Graham and Robert Morris.  Permission
;;; to use it is granted under the Perl Foundations's Artistic License 2.0.
;;; 
;;; Ported to Common Lisp by CHIBA Masaomi.


(defalias disp cl:princ)
(defalias writec cl:write-char)


(def infile (name)
  (cl:open name :direction :input))


(def outfile (name)
  (cl:open name :direction :output
           :if-exists :supersede
           :if-does-not-exist :create))


(def pr args
  (map1 #'disp args)
  (car args))


(def prn args
  (do1 (apply #'pr args)
       (writec #\newline)))


(mac after (x . ys)
  `(unwind-protect ,x ,@ys))

(flet ((expander (f var name body)
         `(let ,var (,f ,name)
               (after (do ,@body) (close ,var)))))
  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )


(mac w/outstring (var . body)
  `(let ,var (make-string-output-stream) ,@body))


(defalias inside cl:get-output-stream-string)


(mac w/stdout (str . body)
  `(let cl:*standard-output* ,str ,@body))


(mac w/stdin (str . body)
  `(let *standard-input* ,str ,@body))


(mac tostring body
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))


(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))




