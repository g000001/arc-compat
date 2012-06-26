(in-package :arc-compat.internal)

(def acons (x) (is (type x) 'cons))

;; is val [val ...]
(defun is (val &rest vals)
  "Tests equality with eqv?"
  (cl:flet ((eqfn (x y)
	      (cl:or (cl:eql x y) (cl:and (cl:stringp x)
				    (cl:stringp y)
				    (cl:string= x y)))))
    (cl:every (fn (_) (eqfn val _))
	      vals)))

;>(is 1 2)
;nil

;>(is "a" "a")
;t

;>(is '(1) '(1))
;nil

;>(is 1 1 1 1)
;t
;
;>(is nil '())
;t


;; isa x y
(defalias isa cl:typep
  "Tests if x has type y.")

;; testify test
(defun testify (x)
  "Creates a predicate from test. If test is a function, it is used
as the predicate. Otherwise, a function is created to test
equality with test using 'is'."
  (if (isa x 'fn) x (fn (_) (is _ x))))


(def iso (x y)
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))

(def x>y (x y)
  (etypecase x
    (cl:number (cl:> x y))
    ((or cl:string cl:symbol) (cl:string> x y))
    (cl:character (cl:char> x y))))

(def x<y (x y)
  (etypecase x
    (cl:number (cl:< x y))
    ((or cl:string cl:symbol) (cl:string< x y))
    (cl:character (cl:char< x y))))

(def > args
  (not (null (cl:reduce #'x>y args))))

(def <= args
  (null (cl:reduce #'x>y args)))

(def < args
  (not (null (cl:reduce #'x<y args))))

(def >= args
  (null (cl:reduce #'x<y args)))


(def even (obj)
  (cl:evenp obj))


(def odd (obj)
  (cl:oddp obj))


