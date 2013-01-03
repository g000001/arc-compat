(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


;;;                           Predicates
;;;============================================================================


(def x>y (x y)
  (etypecase x
    (cl:number (cl:> x y))
    ((cl:or cl:string cl:symbol) (and (cl:string> x y) t))
    (cl:character (cl:char> x y))))


(def x<y (x y)
  (etypecase x
    (cl:number (cl:< x y))
    ((cl:or cl:string cl:symbol) (and (cl:string< x y) t))
    (cl:character (cl:char< x y))))


(define-compiler-macro x<y (&whole form x y)
  (cond ((some #'numberp (cdr form))
         `(cl:< ,x ,y))
        ((some #'characterp (cdr form))
         `(cl:char< ,x ,y))
        ((some #'stringp (cdr form))
         `(and (cl:string< ,x ,y) t))
        (T form)))


(def < args
  "Less than comparison. Applies to numbers, strings, symbols, or chars. 
  If multiple arguments are given, the sequence must be monotonically increasing."
  (cl:every #'x<y args (cdr args)))


(define-compiler-macro < (&whole form &rest args)
  (destructuring-bind (x y &rest rest)
                      args
    (if (null rest)
        `(x<y ,x ,y)
        form)))


(tst <
  (== (< 1 2)
      t )
  (== (< 1 2 3)
      t )
  (== (< 1 3 2)
      nil )
  (== (< "a" "b")
      t )
  (== (< 'a 'b)
      t )
  (== (< #\a #\b)
      t ))


(def > args
  "Greater than comparison. Applies to numbers, strings, symbols, or chars. 
  If multiple arguments are given, the sequence must be monotonically decreasing."
  (cl:every #'x>y args (cdr args)))


(tst >
  (== (> 1 2)
      nil )
  (== (> 3 1 2)
      nil )
  (== (> "a" "b")
      nil )
  (== (> 'a 'b)
      nil )
  (== (> #\a #\b)
      nil ))


(def <= args
  "The less-than-or-equal predicate. It can take an arbitrary number of 
  arguments. There is no short-circuiting; all arguments are evaluated. 
  Arguments must be comparable. Arguments can be numbers, characters or strings,
  but all arguments must be of the same type."
  (case (len args)
    0 (cl:error "invalid number of arguments: ~S" 'args)
    (cl:notany #'x>y args (cdr args))))


(tst <= 
  (== (<= 2 2.0)
      t )
  (== (<= 1 2 3 4 5)
      t )
  (== (<= 1 2 3 5 4)
      nil )
  (== (<= 1 3 2 5 4)
      nil )
  (== (<= #\a #\c #\e)
      t )
  (== (<= "foo" "bar")
      nil ))


(def >= args
  "The greater-than-or-equal predicate. It can take an arbitrary number of
  arguments. There is no short-circuiting; all arguments are evaluated. Arguments
  must be comparable. Arguments can be numbers, characters or strings, but all
  arguments must be of the same type."
  (case (len args)
    0 (cl:error "invalid number of arguments: ~S" 'args)
    (cl:notany #'x<y args (cdr args))))


(tst >= 
  (== (>= 2 3)
      nil )
  (== (>= 3 2 1 1)
      T )
  (== (>= 3 1 2 1)
      nil )
  (== (>= #\a #\c #\e)
      nil )
  (== (>= "baz" "bar")
      t ))


(defalias bound cl:boundp
  "Tests is a symbol is bound.")


(defvar .bound-test-ignore.)
(tst bound
  (== (bound (uniq))
      nil )
  (== (do 
          (set .bound-test-ignore. 1)
          (bound '.bound-test-ignore.) )
      t ))


(defalias exact cl:integerp
  "Tests if the value is an exact integer.")


(tst exact
  (== (exact 3)
      t)
  (== (exact 3.14)
      nil))


(defun is (val &rest vals)
  "Tests equality with eqv?"
  (cl:flet ((eqfn (x y)
	      (cl:or (cl:eql x y) (cl:and (cl:stringp x)
				    (cl:stringp y)
				    (cl:string= x y)))))
    (cl:every (fn (_) (eqfn val _))
	      vals)))


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


(defalias isa cl:typep
  "Tests if x has type y.")


(tst isa
  (== (isa (list 1) 'cons) T)
  (== (isa 'foo 'sym) T)
  (== (isa (lambda ()) 'fn) T)
  (== (isa #\a 'char) T)
  (== (isa "foo" 'string) T)
  (== (isa 1 'int) T)
  (== (isa 1.0 'num) T)
  (== (isa (make-hash-table) 'table) T)
  (== (isa (make-string-input-stream "foo") 'input) T)
  (== (isa (make-string-output-stream) 'output) T))


(def dotted (x)
  "Returns true if x is a dotted list."
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))


(tst dotted
  (== (dotted (cons 1 2))
      t)
  (== (dotted (list 1 2))
      nil))


(def orf fns
  "Creates a function on one variable that tests if any of the original 
  functions are true when applied to the variable."
  (fn (x) (some (fn (_) (funcall _ x)) fns)))


(tst orf
  (== (mapcar (orf #'stringp #'numberp) (list 1 "2" 3 "4" nil) )
      (list T T T T NIL)))


(def andf fns
  "Creates a function on one variable that tests if all of the original 
  functions are true when applied to the variable."
  (fn (x) (cl:every (fn (_) (funcall _ x)) fns)))


(tst andf
  (== (mapcar (andf #'stringp (fn (x) (digit-char-p (elt x 0))))
              (list "f" 1 "2" 3 "4" nil) )
      (list NIL NIL T NIL T NIL)))


(defun testify (x)
  "Creates a predicate from test. If test is a function, it is used
  as the predicate. Otherwise, a function is created to test
  equality with test using 'is'."
  (if (isa x 'fn) x (fn (_) (is _ x))))


(tst testify
  (== (cl:member-if (testify #\a) (list #\z #\z #\a))
      (list #\a)))


(def acons (x) 
  "Tests if x is of type 'cons, i.e. if x is a non-nil list."
  (is (type x) 'cons))


(tst acons
  (== (acons ())
      NIL)
  (== (acons 1)
      NIL)
  (== (acons (list 1))
      T))


;;; atom


(def alist (obj)
  "Tests if x is a list, i.e. nil or of type 'cons. The alist and acons 
  predicates are the same except for nil, which is a list but an atom, 
  not acons."
  (cl:listp obj))


;;; some


(defalias all cl:every
  "Tests if all elements of seq satisfies test. The sequence is either a list or
   a string. The test is a predicate or value, which is wrapped in testify.")


(def isnt (x y)
  "Tests inequality; opposite of is."
  (no (is x y)))


(def iso (x y)
  "Compares x and y. If they are lists, they are compared element-by-element."
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))


(tst iso
  (== (iso (list 1 2 3 4)
           (list 1 2 3 4))
      t)
  (== (iso (list (list 1) 2 3 4)
           (list (list 1) 2 3 4))
      t))


(def atend (i s)
  "Tests if index i is at the end or beyond in sequence or string s."
  (> i (- (len s) 2)))


(tst atend 
  (== (atend 10 "foo")
      T )
  (== (atend 1 "foo")
      NIL ))


(def empty (seq)
  "Tests if seq is empty. Works on lists, strings, and tables."
  (or (not seq)
      (and (no (acons seq)) (is (len seq) 0))))


(tst empty
  (== T (empty ()))
  (== T (empty ""))
  (== T (empty (make-hash-table)))
  (== NIL (empty (list 1 2 3)))
  (== NIL (empty "123"))
  (== NIL (empty (let tab (make-hash-table) (setf (gethash 1 tab) 1) tab))) )


;;; eof
