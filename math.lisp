(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)

;;;                              Math
;;;============================================================================


;;; *


;;; -


;;; /


;;; expt


;;; mod


;;; rand


;;; sqrt


(def trunc (num)
  "Truncates to an integer. Was 'truncate' in arc0."
  (values (cl:truncate num)))


(tst trunc
  (== (trunc -1.1)
      -1))


(defalias number cl:numberp
  "Tests if n is a number (int or num).")


(def positive (x)
  "Tests if x is a number and is positive (>= 0). This fails on complex numbers."
  (and (number x)
       (cl:plusp x)))


(tst positive
  (== (positive 0)
      nil )
  (== (positive -2)
      nil )
  (== (positive "a")
      nil ))


;;; abs


;;; round


(def roundup (n)
  "Rounds n to the nearest value. Halfs are rounded away from zero."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
        (funcall (if (> n 0) #'+ #'-) base 1)
        base)))


(tst roundup
  (== (roundup 1/2)
      1 )
  (== (roundup 1.5)
      2 )
  (== (roundup 0.4)
      0 )
  (== (roundup -1.5)
      -2 ))


(def to-nearest (n quantum)
  "Rounds down to the nearest multiple of n."
  (* (roundup (/ n quantum)) quantum))


(def nearest (n quantum)
  "Rounds down to the nearest multiple of n. Was 'to-nearest' in arc0-2."
  (* (roundup (/ n quantum)) quantum))


(tst nearest
  (== (nearest 3.14d0 0.1d0)
      3.1d0)
  (== (nearest -5 10)
      -10))


(def median (ns)
  "Returns the median of the list (the element at the midpoint of the list when 
  sorted)."
  (ref (sort #'> ns) (trunc (/ (len ns) 2))))


;;; tst :: median -> test-after


(def avg (ns) 
  "Computes the average of a list of numbers."
  (/ (apply #'+ ns) (len ns)))


(tst avg
  (== (avg (list #C(1 2) 0.4d0))
      #C(0.7d0 1.0d0)))


(def even (obj)
  "Tests if n is even. Argument n must be an integer."
  (cl:evenp obj))


(def odd (obj)
  "Tests if n is odd. Argument n must be an integer."
  (cl:oddp obj))


(def best (f seq)
  (if (cl:null seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (funcall f elt wins) (= wins elt)))
        wins)))


(tst best
  (== (best #'> '(3 1 4 5 9 6))
      9))





(def max args 
  "Returns the maximum arg. The args must be comparable with >."
  (best #'> args))


(tst max
  (== (max 10 -3 5)
      10 )
  (== (max "cat" "dog" "bird")
      "dog" ))


(def min args
  "Returns the minimum arg. The args must be comparable with <."
  (best #'< args))


(tst min
  (== (min 10 -3 5)
      -3)
  (== (min "cat" "dog" "bird")
      "bird"))


(def multiple (x y)
  "Tests if x is a multiple of base."
  (is 0 (mod x y)))


(tst multiple
 (== (multiple 10 5)
     t)
 (== (multiple 11 5)
     nil))


;;; eof






