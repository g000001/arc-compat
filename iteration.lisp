(in-package :arc-compat.internal)
(in-suite arc-compat)

;;; Iteration

;; while test [body ...]
(mac while (test &body body)
  "Executes body repeatedly while test is true. The test is
evaluated before each execution of body."
  (w/uniq =>
    `(prog ()
       ,=> (unless ,test (return))
	   ,@body
	   (go ,=>))))

;>(let ((x 0)) (while (< x 3) (print x) (++ x)))
;0
;1
;2

;nil

;; until test [body ...]
(mac until (test &body body)
  "Executes body repeatedly until test is true. The test is
evaluated before each execution of body. until is the opposite of
while."
  (w/uniq =>
    `(prog ()
       ,=> (when ,test (return))
	   ,@body
	   (go ,=>))))

;>(let ((x 0)) (until (> x 3) (print x) (++ x)))
;0
;1
;2
;3
;
;nil

(mac whilet (var test &body body)
  "Executes body repeatedly while test is true. The value of test
is assigned to var on each iteration, for use in the
body. Typically test obtains a new value from some source, and
whilet is used to loop until nil is obtained."
  (w/uniq =>
    `(block nil
       (let ,var nil
	 (tagbody
	,=> (setq ,var ,test)
	    (unless ,var (return))
	    ,@body
	    (go ,=>))))))

#|(with-input-from-string (in "abc")
  (whilet c (read-char in nil nil)
    (princ c)))|#

;
;>(w/instring s "abc" (whilet c (readc s) (prn c)))
;a
;b
;c
;
;nil

;[code] [Macro] whiler var expr endval [body ...]
;Executes body repeatedly while expr is not endval. The value of expr is assigned to var on each iteration.

(mac whiler (var expr endval &body body)
  (w/uniq =>
    `(block nil
       (let ,var nil
	 (tagbody
	,=> (setq ,var ,expr)
	    (when (eql ,var ,endval) (return))
	    ,@body
	    (go ,=>))))))

#|(with-input-from-string (in "abcdef")
  (whiler c (read-char in nil nil) #\d
      (princ c)))|#

;>(w/instring s "abcdef" (whiler x (readc s) #\d (prn x)))
;a
;b
;c
;
;nil
;
;[code] [Macro] loop start test update [body ...]
;Executes start, then executes body repeatedly, checking test before each iteration and executing update afterward.
;	
;
;>(loop (= x 0) (< x 3) (++ x) (prn x))
;0
;1
;2
;
;nil

;; drain expr [eof]
(mac drain (expr &optional eof)
  "Repeatedly executes expr until it returns eof (default nil). 
A list of the expr values is returned."
  (w/uniq (var acc => ans)
    `(block nil
       (let ,ans (list () )
         (with (,var nil ,acc ,ans)
           (tagbody
             ,=> (setq ,var ,expr)
                 (when (eql ,var ,eof) (return (cdr ,ans)))
                 (rplacd ,acc (setq ,acc (list ,var)))
                 (go ,=>)))))))

#|(with-input-from-string (s "(1 2) (3 4)")
  (drain (read s nil)))|#
;
;>(w/instring s "(1 2) (3 4)" (drain (sread s nil)))
;((1 2) (3 4))
;
;>(let x 256 (drain (= x (/ x 2)) 1))
;(128 64 32 16 8 4 2)

;[code] [Macro] for var init max [body ...]
;

(defmacro for (var init max &body body)
  "Executes body repeatedly with var assigned the values from init to max,
  incremented by 1 each time. For the last iteration, v will be >= max. 
  If max <= init-1, body will not be executed."
  (w/uniq (=> block _max)
    `(block ,block
       (cl:let ((,var ,init)
                (,_max (1+ ,max)))
         (tagbody
           ,=> (cond ((>= ,var ,_max)
                      (return-from ,block)))
               (progn ,@body)
               (incf ,var)
               (go ,=>))))))


(tst for
  (== (with-output-to-string (out)
        (for x 2.5 4.0 (princ x out)))
      "2.53.54.5")
  (== (with-output-to-string (out)
        (for x 2.5 1.6 (princ x out)))
      "2.5"))


;[code] [Macro] repeat n [body ...]
;Executes body n times. Non-integers are rounded up.
(mac repeat (n &body body)
  (w/uniq (var =>)
    `(prog ((,var ,n))
       ,=> (when (>= 0 ,var) (return))
           ,@body
	   (setq ,var (cl:1- ,var))
	   (go ,=>))))

;>(repeat 3 (print "hi"))
;hi
;hi
;hi
;
;nil
;
;[code] [Macro] forlen var seq [body ...]
;Iterates over a sequence (list, string, or table) seq. var takes the values from 0 to length-1.
;	
;
;>(let seq '(1 2 3) (forlen x seq (prn x " " (seq x))))
;0 1
;1 2
;2 3
;
;nil
;
;>(let seq "abc" (forlen x seq (prn x " " (seq x))))
;0 a
;1 b
;2 c
;
;nil
;
;>(let seq (obj 0 'val0 1 'val1)
;  (forlen x seq (prn x " " (seq x))))
;0 val0
;1 val1
;
;nil
;
;[code] [Macro] each var expr [body ...]
;Executes body, with var taking on each value from expr, which can be a list, string, or table. For a table, var takes on the values, not the keys.

(mac each (var expr . body)
  (w/uniq (gseq g)
    `(let ,gseq ,expr
       (if (alist ,gseq)
            (funcall 
             (afn (,g)
               (when (acons ,g)
                 (let ,var (car ,g) ,@body)
                 (self (cdr ,g))))
             ,gseq)
           (isa ,gseq 'table)
            (maptable (fn (,g ,var) ,@body)
                      ,gseq)
            (for ,g 0 (- (len ,gseq) 1)
              (let ,var (cl:elt ,gseq ,g) ,@body))))))


;
;>(each x '(1 (2 3) 4) (prn x))
;1
;(2 3)
;4
;
;nil
;
;>(each x "abc" (prn x))
;a
;b
;c
;
;nil
;
;>(each x (obj key1 'val1 key2 'val2) (prn x))
;val1
;val2
;
;#hash((key2 . val2) (key1 . val1))
;
;[code] [Macro] noisy-each n var val [body ...]
;Version of each, printing a dot every n iterations.
;	
;
;>(noisy-each 3 x "abcdefghijk" (pr x))
;ab.cde.fgh.ijk
;
;nil
;
;[code] [Macro] on var s [body ...]
;Iterates the same as each, except the variable index is assigned a count, starting at 0. For tables, var is assigned nil each iteration, so ontable is probably more useful.
;	
;
;>(on x '(1 (2 3) 4) (prn index " " x))
;0 1
;1 (2 3)
;2 4
;
;nil
;
;>(on x "abc" (prn index " " x))
;0 a
;1 b
;2 c
;
;nil
;
;>(on x (obj key1 'val1 key2 'val2) (prn index " " x))
;0 nil
;1 nil
;
;nil
;
;[code] [Macro] ontable k v tab [body ...]
;Iterates over the table tab, assigning k and v each key and value.
;	
;
;>(ontable k v (obj key1 'val1 key2 'val2) (prn k " " v))
;key1 val1
;key2 val2
;
;#hash((key2 . val2) (key1 . val1)))
