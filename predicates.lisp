(in-package :arc)

;; is val [val ...]
(defun is (val &rest vals)
  "Tests equality with eqv?"
  (cl:flet ((eqfn (x y)
	      (or (cl:eql x y) (and (cl:stringp x)
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