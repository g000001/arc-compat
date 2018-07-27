(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(tst median
  (== (median (list 1 5 3 2 4))
      3 )
  (== (median (list "dog" "cat" "bird"))
      "cat" ))


(tst trunc
  (== (trunc -1.1)
      -1))


(tst atomic-invoke
  (dotimes (i 5)
    (== (cl:with-output-to-string (out)
          (cl:let ((ths '() )
                   (cnt 0))
            (dotimes (i 100)
              (push (bt:make-thread 
                     (lambda () 
                       (atomic-invoke (lambda ()
                                        (sleep (random .02))
                                        (princ cnt out)
                                        (++ cnt)) )))
                    ths ))
            (mapc #'bt:join-thread ths) ) )
        "0123456789101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899")))


(tst timedate
  (== (timedate 0)
      (list 0 0 0 1 1 1900)))


(tst scar
  (== (let x nil
        (= x (copy "abc"))
        (scar x #\d)
        x)
      "dbc")
  (== (let x nil
        (= x (list 1 2 3))
        (scar x #\d)
        x)
      (list #\d 2 3)))


(tst scdr
  (== (let x nil
        (= x (list 1 2 3))
        (arc:scdr x (list 4))
        x)
      (list 1 4)))


(tst is
  (== (is 1 2)
      nil )
  (== (is "a" "a")
      t )
  (== (is (list 1) (list 1))
      nil )
  (== (is 1 1 1 1)
      t )
  (== (is nil '())
      t ))


(tst len
  (== (len "abc")
      3)
  (== (len '(1 2 3))
      3)
  (== (len (obj a 1 b 2))
      2))

;;; 
;;; ar.lisp
;;; 

(tst sig
  (== (sig #'len) '(SEQ))
  (== (sig 'let) '(var val &body body)))


(tst err
  (>_< (cl:error) (err "foo")))


(tst x+y
  ;; compiler-macro
  (== (x+y (cl:the cl:fixnum (values 8))
           (cl:the cl:fixnum (values 8)))
      16))


(tst type
  (== (type #'list) 'FN)
  (== (type 1.0) 'NUM)
  (== (type 1.0) 'NUM)
  (== (type (stdout)) 'OUTPUT))


(tst writeb
  ;; FIXME
  (== (cl:with-open-stream (*standard-output*
                            (ironclad::make-octet-output-stream))
        (writeb 8))
      8))


(tst coerce
  (== (cl:map 'cl:string
              (fn (_) (coerce (coerce _ 'int) 'char))
              "abcdefg")
      "abcdefg")
  (== (coerce "" 'cons)
      nil)
  (== (coerce #\Z 'string) "Z")
  (== (coerce "" 'cl:list)
      nil)
  (== (coerce "123" 'int)
      123)
  (== (coerce '(#\A #\b #\c) 'string)
      "Abc")
  (>_< (cl:error) (coerce '(#\a #\b #\c) 'int))
  (== (coerce "foo" 'sym)
      '|foo|)
  (>_< (cl:error) (coerce "foo" (uniq))))


(tst timedate
  (== (cdr (timedate))
      (cdr (timedate)))
  (== (timedate 0)
      (timedate 0)))


(tst call-w/stdout
  (== (let sop (outstring)
        (call-w/stdout sop (fn () (prn '(1 2))))
        (inside sop))
      "(1 2)
"))


(tst call-w/stdin
  (== (call-w/stdin (instring "Hello") #'cl:read-line)
      "Hello"))


(tst protect 
  (== (tostring 
       (protect (fn () (/ 1 0))
                (fn () (pr "after"))))
      "after"))


(tst details
  (== "foo 42"
      (handler-case (error "foo ~A" 42)
        (error (c) (details c)))))


(tst sref
  (== (let x "abc__"
        (sref x #\d 1)
        x)
      (copy "adc__"))
  (== (let x (list 1 2 3)
        (sref x #\d 1)
        x)
      (list 1 #\d 3)))


(tst ccc
  (== (ccc (fn (ep) (funcall ep "bailout value") 42))
      "bailout value")
  (== (ccc (fn (ep) (list ep "bailout value") 42))
      42))


;;; eof
