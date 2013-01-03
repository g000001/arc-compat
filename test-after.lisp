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
        (= x "abc")
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


;;; eof
