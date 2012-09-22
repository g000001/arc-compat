(in-package :arc-compat.internal)
(in-readtable :common-lisp)

;  [code] [Foundation] [Destructive] set symbol expr
(defmacro set (&rest args)
  "set is used to set a variable to an expression."
  `(cl:setf ,@args))

;>(set x 10)
;10

(defalias = set)

;[code] [Foundation] [Destructive] scar list expr

(defmacro scar (list expr)
  "Sets car of list to a new expression. If applied to a string,
sets the first character of the string, which must have length at
least one."
  `(etypecase ,list
     (cl:list (setf (car ,list) ,expr))
     (cl:string (setf (aref ,list 0) ,expr))))

;>(do
;    (= x '(1 2 3))
;    (= x "abc")
;    (scar x #\d)
;    x)
;"dbc"
;
;>(do
;    (= x '(1 2 3))
;    (scar x #\d)
;    x)
;(#\d 2 3)

;[code] [Foundation] [Destructive] scdr list exp

(defalias scdr cl:rplacd
  "Sets cdr of a list.")
	
;>(do
;    (= x '(1 2 3))
;    (scdr x '(4))
;    x)
;(1 4)

;[code] [Macro] [Destructive] = [place expr] ... [place]
(defalias = cl:setf
  "Sets each place to the associated expression.  If the last place
has no associated expression, it is set to nil.")

;>(= x 1)
;1

;>(= x 2 y 4)
;4

;;(w/table t1 (= (t1 "a") 42) (write-table t1))


;[code] [Macro] [Destructive] wipe [place ...]
;
(mac wipe (&rest args)
  "Sets the places to nil. Typically, the places are simple
variables."
  `(do ,@(mapcar (fn (a) `(= ,a nil))
		 args)))

;(do (wipe a b c) (list a b c))
;(nil nil nil)

;[code] [Macro] [Destructive] assert [place ...]
(mac assert (&rest args)
  "Sets the places to t. Note that this is unrelated to asserting
that a condition holds."
  `(do ,@(mapcar (fn (a) `(= ,a t))
		 args)))

;>(do (assert a b c) (list a b c))
;(t t t)

;[code] [Macro] [Destructive] swap place1 place2
;The contents of the two places are swapped. The new contents of place2 are returned.

;>(with (x 'a y '(1 2))
;  (swap x y)
;  (prn "x:" x)
;  y)
;x:(1 2)

;[code] [Macro] [Destructive] rotate [place1 place2 ...]

(defalias rotate cl:rotatef
  "Assigns place2 to place1, assigns place3 to place2, and so on,
assigning place1 to the last place.")

#|(let ((s "abc"))
  (rotate (aref s 0)
	   (aref s 1)
	   (aref s 2))
  s)|#

;>(let s "abc" (rotate (s 0) (s 1) (s 2)) s)
;"bca"

;[code] [Macro] [Destructive] ++ place [i]
(defalias ++ cl:incf
  "Increments the value at place by i. The default increment is 1.")

;>(let ((x '(10 20))) 
;   (++ (car x))
;   (++ (cadr x) 5)
;   x)
;(11 25)

;[code] [Macro] [Destructive] -- place [i]

(defalias -- cl:decf
  "Decrements the value at place by i. The default decrement is 1.")	

;>(let ((x '(10 20))) (-- (car x)) (-- (cadr x) 5) x)
;(9 15)

;[code] [Macro] [Destructive] zap op place [args ...]
"Gets the value at the place, evaluates (op value args...), and stores the result in the place."
(mac zap (op place &rest args)
  `(setf ,place (apply ,op ,place (list ,@args))))

#|(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map (fn (_) (declare (ignore _)) (uniq)) args)
         mix    (afn seqs 
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))|# 


;(let ((x '(0 10 20))) (zap #'* (cadr x) 5 8 9 10) x)

;>(let x '(0 10 20) (zap * (x 1) 5) x)
;(0 50 20)

;[code] [Macro] [Destructive] push elt place
;Pushes an element at the beginning of the list referenced by place. The list is modified and returned.

;>(let x '(1 2 3) (push 'a x) x)
;(a 1 2 3)

;[code] [Macro] [Destructive] pushnew elt place [test]
;Pushes elt before the place if it is not present in the list. The equality test can be specified; iso is used by default.

;>(let x '(1 2 3) (pushnew 'a x) x)
;(a 1 2 3)

;>(let x '(1 2 3) (pushnew 2 x) x)
;(1 2 3)

;[code] [Macro] [Destructive] pop place
;The first element is removed from place and returned. If the value at the place is nil, then nil is returned.
	
;>(let x '(1 2 3)
;  (prn "Popped:" (pop x))
;  x)
;Popped:1

;(2 3)

;[code] [Macro] [Destructive] pull test place

(defmacro pull (test place)
  "Remove elements satisfying test from the list starting at place."
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'cl:list vars forms)
              (,(car var) (delete-if ,test ,access)))
         ,set)))

;(let ((x '((1 100 2 50 3) foo)))
;  (pull (fn (x)(< x 10)) (car x))
;  x)

;>(let x '(1 100 2 50 3) (pull [< _ 10] x) x)
;(100 50)
