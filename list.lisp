(in-package :arc-compat.internal)

;; car list

;; cdr list

;; caar list

;; cadr list

;; cddr list

;; conswhen f x y
(defun conswhen (f x y)
  "Cons x and y if (f x) is true. Otherwise returns y."
  (if (funcall f x)
      (cons x y)
      y))

;>(conswhen (fn (_) (< (len _) 3)) '(1 2) '(3 4 5))
;((1 2) 3 4 5)
;
;>(conswhen (fn (_) (< (len _) 3)) '(1 2 3 4) '(3 4 5))
;(3 4 5)

;; consif x list
(defun consif (x list)
  "Cons x onto list if x is not nil."
  (if x (cons x list) list))
;
;>(consif 1 '(2 3))
;(1 2 3)
;
;>(consif nil '(2 3))
;(2 3)

;[code] [Procedure] firstn n list
;Returns the first n elements of list.
(defun firstn (n list)
  "Returns first 'n' elements of list."
  (cl:loop :repeat n :for x :in list :collect x))
;
;>(firstn 3 '(1 2))
;(1 2)
;
;>(firstn 3 '(a b c d e))
;(a b c)

;; nthcdr n list <- *cl*
#|(defun nthcdr (n list)
  "Returns list after skipping the first n elements."
  (if (> 1 n)
      list
      (cl:nthcdr n list)))|#

;> (nthcdr -88 '(1 2 3))
;>(nthcdr 0 '(1 2 3))
;(1 2 3)
;
;>(nthcdr 2 '(1 2 3))
;(3)
;
;>(nthcdr 10 '(1 2 3))
;nil
;

(def last (seq)
  "Returns the last element of list."
  (if (no (cdr seq))
      (car seq)
      (last (cdr seq))))

;>(last '(1 2 3))
;3
;
;[code] [Procedure] flat list [stringstoo]
;
(def flat (x (o stringstoo))
  "Flattens list into a list of atoms. Any nils are removed. If stringstoo is true, empty strings are removed, but flat will fail if the list contains any non-empty strings."
  (funcall
   (rfn f (x acc)
     (if (or (no x) (and stringstoo (is x "")))
         acc
         (and (atom x) (no (and stringstoo (isa x 'string))))
         (cons x acc)
         (f (car x) (f (cdr x) acc))))
   x nil))
;	
;
;>(flat '(1 2 () (3 4 (5))))
;(1 2 3 4 5)
;
;; rev list
;
(defalias rev cl:reverse
  "Reverses list.")

;>(rev '(1 2 3))
;(3 2 1)
;
;>(rev '(1 (2 3) 4))
;(4 (2 3) 1)
;

;; carif x
(defun carif (x)
  "Returns (car x) if x is a list, and returns x otherwise. This
provides a 'safe' way to return the first element of something
that may or may not be a list."
  (if (atom x) x (car x)))

;>(carif '(1 2))
;1
;
;>(carif 3)
;3

;; caris x val
(defun caris (x val)
  "Tests if x is a list and (car x) is val."
  (eql val (if (atom x) nil (car x))))
;
;>(caris '(1 2) 1)
;t
;
;>(caris 1 1)
;nil

;; intersperse x list
(defun intersperse (x ys)
  "Inserts x between elements of list. If list has fewer than 2
elements, there is no effect."
  (cons (car ys)
        (mappend (fn (_) (list x _)) 
		 (cdr ys))))

;>(intersperse 1 '(a b (c d) e))
;(a 1 b 1 (c d) 1 e)
;
;>(intersperse nil '(1 2 3))
;(1 nil 2 nil 3)
;
;[code] [Procedure] split list pos
;Splits list into two lists at the given position, which must be between 1 and the length of the list.
(defun split (seq pos)
  (withs (mid (nthcdr (- pos 1) seq)
          s2  (cdr mid))
    (wipe (cdr mid))
    (list seq s2)))

;>(split '(a b c) 0)
;((a) (b c))
;
;>(split '(a b c) 1)
;((a) (b c))
;
;>(split '(a b c) 2)
;((a b) (c))
;
;>(split '(a b c) 3)
;((a b c) nil)
;

;; pair list [f]
(defun pair (xs &optional (f #'list))
  "Splits list into pairs. By default, each pair is made into a
list. If specified, function f is applied to each pair."
  (cl:cond ((no xs) nil)
	   ((no (cdr xs)) (list (list (car xs))))
	   ('T (cons (funcall f (car xs) (cadr xs))
		     (pair (cddr xs) f)))))

;>(pair '(a b c d))
;((a b) (c d))
;
;>(pair '(a b c d e))
;((a b) (c d) (e))
;
;>(pair '(1 2 3 4) #'+)
;(3 7)
;
;>(pair '(10 2 3 40 50 6) max)
;(10 40 50)

;; tuples list [n]
(defun tuples (xs &optional (n 2))
  "Splits list into groups of n. tuples is a generalization of
pair."
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

;>(tuples '(1 2 3 4 5) 1)
;((1) (2) (3) (4) (5))
;
;>(tuples '(1 2 3 4 5))
;((1 2) (3 4) (5))
;
;>(tuples '(1 2 3 4 5) 3)
;((1 2 3) (4 5))

;; join [list ...]
(defun join (&rest args)
  "Joins lists into one list."
  (if (no args)
      nil
      (let a (car args) 
        (if (no a) 
            (apply #'join (cdr args))
            (cons (car a) (apply #'join (cdr a) (cdr args)))))))

;>(join '(1 2) nil '(3 4))
;(1 2 3 4)

;; list [arg ...] <- *cl*

;; range start end
(defun range (start end)
  "Creates a list of numbers from start to end in steps of 1. The
last number is <= end."
  (cl:loop :for i :from start :to end :collect i))

;>(range 0 10)
;(0 1 2 3 4 5 6 7 8 9 10)
;
;>(range 1.5 3.8)
;(1.5 2.5 3.5)

;; n-of n expr
(mac n-of (n expr)
  "Evaluates expr n times and returns a list of the results."
  `(let res ()
     (repeat ,n
       (push ,expr res))
     (rev res)))

;(with-input-from-string (ins "abcdefg")
;  (n-of 5 (read-char ins)))
;>(n-of 5 "a")
;("a" "a" "a" "a" "a")
;
;>(w/instring ins "abcdefg" (n-of 5 (readc ins)))
;(#\a #\b #\c #\d #\e)

(def adjoin (x xs (o test #'iso))
  "Cons elt onto list unless (test elt y) is true for some y in
list. By default, test is iso, so elt will be joined if it is not
present in list."
  (if (some (fn (_) (funcall test _)) xs)
      xs
      (cons x xs)))


;>(adjoin 2 '(1 2 3))
;(1 2 3)
;
;>(adjoin 2 '(1 3 5))
;(2 1 3 5)
;
;>(adjoin 2 '(1 2 3) <)
;(1 2 3)
;
;>(adjoin 2 '(0 1 2) #'<)
;(2 0 1 2)
;
;[code] [Procedure] [Destructive] counts list [table]
;
#|(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (zap (fn (_) (if _ (cl:+ _ 1) 1))
               (funcall c (car seq)))
          (counts (cdr seq) c))))|#


(def counts (seq (o c (table)))
  "Counts how many times each element of list occurs. The results are returned as a table mapping from the element to the count. If a table is passed in, it will be updated."
  (if (no seq)
      c
      (do (let key (car seq)
            (multiple-value-bind (val win)
                                 (gethash key c)
              (declare (ignore val))
                 (if win 
                     (incf (gethash key c))
                     (setf (gethash key c) 1))))
          (counts (cdr seq) c))))

;	
;
;>(counts '(b a n a n a))
;#hash((n . 2) (a . 3) (b . 1))
;
;>(let tl (table)
;  (counts '(1 2) tl)
;  (counts '(1 3) tl))
;#hash((3 . 1) (1 . 2) (2 . 1))


#|(let tl (table)
     (counts '(1 2) tl)
     (counts '(1 3) tl)
     (maphash (fn (k v) (print (list k v))) tl))|#

;[code] [Procedure] commonest list
;Returns the element of list occurring most frequently, along with its count.

(def commonest (seq)
  (with (winner nil n 0)
    (ontable k v (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

;
;>(commonest '(b a n a n a))
;(a 3)
;
;>(commonest nil)
;(nil 0)
;
;Applying functions to lists
;Arc provides several ways of applying functions to the elements of a list.

;; reduce f list
(defun reduce (f list)
  "Reduces list using f. Applies f to the first two elements of
list. Then recursively applies f to that result and the next
element of list."
  (cl:reduce f list))

;>(reduce #'+ '(1 2 3 4 5))
;15
;
;>(reduce #'+ '("a" "b" "c"))
;"abc"
;
;>(reduce #'/ '(1 2 3))
;1/6
;

;; rreduce f list
;
(defun rreduce (f list)
  "Reduces list using f in reverse. Applies f to the last two
elements of list. Then recursively applies f to that result and
the previous element of list."
  (cl:reduce f list :from-end 'T))

;>(rreduce #'+ '(1 2 3 4 5))
;15
;
;>(rreduce #'/ '(1 2 3))
;3/2

;; firstn-that n f list
(defun firstn-that (n f xs)
  "Returns the first n elements of list for which predicate f is
true."
  (cl:cond ((or (cl:<= n 0) (no xs)) 
	    nil)
	   ((funcall f (car xs))
	    (cons (car xs) (firstn-that (- n 1) f (cdr xs))))
	   ('T (firstn-that n f (cdr xs)))))

;>(firstn-that 3 #'oddp '(1 2 3 4 5 6 7 8))
;(1 3 5)
;
;>(firstn-that 3 #'oddp '(2 4 6 8))
;nil

;; most f list
(defun most (f seq) 
  "Returns the element of list for which rating function f
returns the largest value."
  (cl:unless (no seq)
    (withs (wins (car seq) topscore (funcall f wins))
      (cl:dolist (elt (cdr seq))
        (let score (funcall f elt)
          (if (cl:> score topscore) (= wins elt topscore score))))
      wins)))

;>(most #'len '("cat" "bird" "dog"))
;"bird"
;
;>(most #'abs '(3 -10 5))
;-10
;
;>(most #'abs '(-1 1 -1))
;-1

;[code] [Procedure] map1 f list

(defalias map1 cl:mapcar
  "Applies f to the elements of list. The results are cons'd
together into a list.")

;>(map1 (fn (_) (list _ (* _ 10))) '(1 2 3))
;((1 10) (2 20) (3 30))
;
;>(map1 #'cdr '((1) (2 3) (4 5)))
;(nil (3) (5))

;; mappend f [list ...]
(defun mappend (f &rest args)
  "Maps f on the arguments, and then joins the results together. f
must return a list. nil results are omitted."
  (apply #'cl:append (apply #'cl:mapcar f args)))

;>(mappend (fn (_) (list _ (* _ 10))) '(1 2 3))
;(1 10 2 20 3 30)
;
;>(mappend #'cdr '((1) (2 3) (4 5)))
;(3 5)

;; reclist f list
(defun reclist (f xs)
  "Recursively applies f to tail subsequences of list and returns
the first true result. Returns nil if none."
  (cl:and xs (cl:or (funcall f xs) (reclist f (cdr xs)))))

;>(reclist (fn (x) (print x) nil) '(a b c))
;(a b c)
;(b c)
;(c)
;
;nil
;
;>(reclist (fn (_) (if (is (len _) 2) _)) '(a b c))
;(b c)

;; mem test list
(defalias mem cl:member-if
  "Tests elements of list. If test is true for an element, returns
the remainder of the list from that point. test is either an
element or a predicate.")

;>(mem (fn (_) (oddp _)) '(2 4 5 6 7))
;(5 6 7)
;
;>(mem 6 '(2 4 5 6 7))
;(6 7)

;[code] [Procedure] trues f list
(defun trues (f seq) 
  "Maps function f onto list and returns only the true (non-nil) values."
  (rem nil (cl:mapcar f seq)))
;
;>(trues #'cdr '((1 2) (3) (4 5)))
;((2) (5))
;
;>(trues (fn (_) (if (oddp _) (* 10 _))) '(1 2 3 4 5))
;(10 30 50)
;
;Sorting
;Arc provides an efficient sorting operation based on merge sort. Sorting in Arc uses a compare predicate function that defines the sort order. Elements x and y are defined as sorted if (compare x y) is true. The compare function does not need to define a full order. That is, it is valid for (compare x y) and (compare y x) to both be true. In this case, mergesort is stable, and will preserve the existing order of the elements.
;
;[code] [Procedure] [Destructive] mergesort compare list

(def merge (less? x y)
  "Merges two sorted lists into a sorted list. The original lists must be ordered according to the predicate function compare."
  (if (no x) y
      (no y) x
      (let lup nil
        (set lup
             (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
               (if (funcall less? (car y) (car x))
                 (do (if r-x? (scdr r y))
                     (if (cdr y) (funcall lup y x (cdr y) nil) (scdr y x)))
                 ; (car x) <= (car y)
                 (do (if (no r-x?) (scdr r x))
                     (if (cdr x) (funcall lup x (cdr x) y t) (scdr x y))))))
        (if (funcall less? (car y) (car x))
          (do (if (cdr y) (funcall lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (funcall lup x (cdr x) y t) (scdr x y))
              x)))))

(def mergesort (less? lst)
  "Destructively sorts list using the given comparison function. The sort is stable; if two elements compare as equal with compare, they will remain in the same order in the output. The original list is destroyed."
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        (funcall 
         (afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (funcall less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

;
;	
;
;(-7 0 3 10)
;(mergesort #'< '(3 0 10 -7))
;=>  (-7 0 3 10)

;
;>(mergesort (fn (a b) (< (len a) (len b)))
;            '("horse" "dog" "elephant" "cat"))
;("dog" "cat" "horse" "elephant")

; (mergesort (fn (a b) (< (len a) (len b)))
;           '("horse" "dog" "elephant" "cat"))
;=>  ("dog" "cat" "horse" "elephant")



;
;>(merge #'< '(1 2 3 5) '(2 4 6))
;(1 2 2 3 4 5 6)
;
;>(merge (fn (a b) (> (len a) (len b)))
;  '("aaa" "b") '("cccc" "ddd" "ee"))
;("cccc" "aaa" "ddd" "ee" "b")
;
;[code] [Procedure] insert-sorted compare elt list
(def insert-sorted (test elt seq)
  "Creates a new list with elt inserted into the sorted list list. The original list must be sorted according to the comparison function. The original list is unmodified."
  (if (no seq)
       (list elt) 
      (funcall test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

;(insert-sorted #'> 5 '(10 3 1))
;=>  (10 5 3 1)

;(insert-sorted #'> 5 '(10 5 1))
;=>  (10 5 5 1)
;(10 5 5 1)
;
;[code] [Macro] [Destructive] insort (compare elt list)
;
(mac insort (test elt seq)
  "Insert elt into previously-sorted list, updating list."
  `(zap (fn (_) (insert-sorted ,test ,elt _)) ,seq))

;>
;(let x '(2 4 6) (insort #'< 3 x) x)
;=>  (2 3 4 6)

;(2 3 4 6)
;
;[code] [Procedure] reinsert-sorted compare elt list
;Creates a new list with elt inserted into the sorted list list if it is not already present. The original list must be sorted according to the comparison function. The original list is unmodified.
;	
;
;>(reinsert-sorted > 5 '(10 3 1))
;(10 5 3 1)
;
;>(reinsert-sorted > 5 '(10 5 1))
;(10 5 1)
;
;[code] [Macro] [Destructive] insortnew (compare elt list)
;Insert elt into previously-sorted list if it is not present, updating list.
;	
;
;>(let x '(2 4 6) (insortnew < 3 x) x)
;(2 3 4 6)
;
;[code] [Procedure] best compare list
;Returns the 'best' element of list as determined by function compare.
;	
;
;>(best > '(3 1 4 5 9 6))
;9
;
;[code] [Procedure] bestn n compare list
;Returns the first n elements of list when sorted according to comparison function compare.
;	
;
;>(bestn 3 > '(3 1 4 5 9 6))
;(9 6 5)
;
;>(bestn 3 < '(3 1 4 5 9 6))
;(1 3 4)
;
;[code] [Procedure] sort test seq
;Sorts the sequence (list or string) using the specified comparison function. The original sequence is unmodified.
;	
;
;>(sort < '(3 1 4 9 0))
;(0 1 3 4 9)
;
;>(sort > "Test word")
;"wtsroedT "
;
;Sequence manipulation
;These operations act on lists, strings, or hash tables.
;
;[code] [Foundation] [Destructive] sref seq value index
;Sets indexed entry in a list, string, or hash table to the given value.
;	
;
;>(do
;    (= x "abc")
;    (sref x #\d 1) x)
;"adc"
;
;>(do
;    (= x '(1 2 3))
;    (sref x #\d 1) x)
;(1 #\d 3)
;
;[code] [Procedure] count test seq
;Counts the number of elements of seq that satisfy test. test is an object or predicate. For a table, the elements are the values.
;	
;
;>(count #\a "banana")
;3
;
;>(count (fn (_) (odd _)) '(1 2 3 4 5))
;3
;
;[code] [Procedure] union f xs ys
;Takes union of sequence xs and ys. Predicate f is used to determine equality to filter out duplicates. xs and ys must both be lists or strings.
;	
;
;>(union is '(1 2 3) '(2 3 4))
;(1 2 3 4)
;
;>(union is "ab" "banana")
;"abnn"
;
;>(union (fn (a b) (is (mod a 10) (mod b 10))) '(1 2 3) '(13 24 35))
;(1 2 3 24 35)
;

;; len seq
(defalias len cl:length
  "Computes the length of seq.")
;	
;
;>(len "abc")
;3
;
;>(len '(1 2 3))
;3
;
;>(len (obj a 1 b 2))
;2
;
;[code] [Procedure] [Predicate] len< seq n
;Tests if length of seq is less than n.
;	
;
;>(len< "abc" 4)
;t
;
;>(len< '(1 2 3) 4)
;t
;
;>(len< (obj a 1 b 2) 4)
;t
;
;[code] [Procedure] [Predicate] len> seq n
;Tests if length of seq is greater than n.
;	
;
;>(len> "abc" 4)
;nil
;
;>(len> '(1 2 3) 4)
;nil
;
;>(len> (obj a 1 b 2) 4)
;nil
;
;[code] [Procedure] dedup seq
;Returns contents of seq without duplicates. For a string, returns a list of characters. For a table, returns a list of values.
;	
;
;>(dedup '(1 2 3 2 1))
;(1 2 3)
;
;>(dedup "abcba")
;(#\a #\b #\c)
;
;>(dedup (obj a 1 b 2 c 1))
;(2 1)
;
;[code] [Procedure] [Predicate] single list
;Returns true if given a list of length one.
;	
;
;>(single '(1))
;t
;
;>(single 1)
;nil
;
;>(single '())
;nil
;
;[code] [Procedure] pos test seq [start]
;Returns the index of the first element of seq that satisfies test. seq is a list or string. test is either an object or predicate function. If start is given, testing starts at that element.
;	
;
;>(pos 'c '(a b c d))
;2
;
;>(pos #\c "abcd")
;2
;
;>(pos #\c "abcdc" 3)
;4
;
;>(pos odd '(2 4 5 6 7))
;2
;
;>(pos odd '(2 4 6))
;nil
;
;[code] [Procedure] [Predicate] before t1 t2 seq [start]
;Tests if t1 is true before t2 in seq. seq is either a list or string. The tests are either objects or predicate functions. If start is given, search starts with the specified element.
;	
;
;>(before 4 odd '(2 4 1 3))
;t
;
;>(before 4 odd '(2 3 4 1))
;nil
;
;>(before #\a #\n "banana")
;t
;
;>(before #\a #\n "banana" 2)
;nil
;
;[code] [Procedure] random-elt seq
;Returns a random element from a list, or a random character from a string. It also works on a table with integer keys from 0 to n.
;	
;
;>(random-elt '(1 2 3))
;3
;
;>(random-elt "abcd")
;#\c
;
;[code] [Procedure] mismatch s1 s2
;Compares sequences and returns the position of the first mismatch (as determined by is). Returns nil if the sequences are identical.
;	
;
;>(mismatch "abcde" "abXde")
;2
;
;>(mismatch '(1 2 3) '(1 9 3))
;1
;
;>(mismatch "abc" "abc")
;nil
;
;[code] [Procedure] find test seq
;Finds and returns the first element of seq that satisfies test (object or predicate). seq can be a list or string.
;	
;
;>(find odd '(2 3 4 5))
;3
;
;>(find odd '(2 4 6))
;nil
;
;>(find alphadig "...abc...")
;#\a
;
;[code] [Procedure] cut seq start [end]
;Returns subsequence of seq from start to end. If end is not specified, the remainder of seq is used. The seq can be a list or string.
;	
;
;>(cut "abcde" 2)
;"cde"
;
;>(cut "abcde" 2 4)
;"cd"
;
;>(cut '(a b c d) 2)
;(c d)
;
;>(cut '(a b c d) 2 4)
;(c d)
;

;; rem test seq
(defalias rem cl:remove-if
  "Removes elements from seq that satisfy test. test is either a
function or an object. seq is either a list or string.")

#|(defun rem (test seq)
  (let f (testify test)
    (if (alist seq)
        (funcall (afn (s)
		   (cl:cond ((no s) nil)
			    ((funcall f (car s)) (self (cdr s)))
                            ('T (cons (car s) (self (cdr s)))))
		   seq)
        (coerce (rem test (coerce seq 'cons)) 'string)))))|#


;>(rem #'oddp '(1 2 3 4 5))
;(2 4)
;
;>(rem 3 '(1 2 3 4 5))
;(1 2 4 5)
;
;>(rem #\c "abcde")
;"abde"
;
;>(rem (fn (_) (in _ #\a #\b)) "abcde")
;"cde"
;
;[code] [Procedure] keep test seq
;Keeps elements from seq that satisfy test. test is either a function or an object. seq is either a list or string.
;	
;
;>(keep odd '(1 2 3 4 5))
;(1 3 5)
;
;>(keep 3 '(1 2 3 4 5))
;(3)
;
;>(keep #\c "abcde")
;"c"
;
;>(keep (fn (_) (in _ #\a #\b)) "abcde")
;"ab"
;
;[code] [Procedure] map f [seq ...]
;Applies f to the elements of the sequences, taking the first from each, the second from each, and so on. If there are n sequences, f must be a function accepting n arguments. The sequences can be lists or strings. If any sequence is a string, then f must return a character, and the result will be a string made up of the results from f. Otherwise, the result will be a list of the results from f. The sequences are processed up to the length of the shortest sequence. For a single list, map is the same as map1.
;	
;
;>(map (fn (a b c) (+ (* a 100) (* b 10) c))
;  '(1 2 3) '(4 5 6) '(7 8 9 10))
;(147 258 369)
;
;>(map (fn (_) (list _ (* _ 10))) '(1 2 3))
;((1 10) (2 20) (3 30))
;
;>(map cdr '((1) (2 3) (4 5)))
;(nil (3) (5))
;
;>(map (fn (c n) (coerce (+ n (coerce c 'int)) 'char)) "abc" '(0 2 4))
;"adg"
;
;>(map min "bird" "elephant")
;"bied"
;
;Other
;
;[code] [Macro] rand-choice expr [...]
;Randomly choose one of the expressions.
;	
;
;>(
;and-choice "a" 42 '(1 2 3))
;(1 2 3)
;
;[code] [Procedure] compare comparer scorer
;Creates a procedure on two values that applies scorer to each value, and then applies comparer to the two scores.
;	
;
;>(compare < len)
;#<procedure>
;
;pre> >((compare < len) "yz" "abc") t
;[code] [Procedure] only f
;Creates a procedure that will apply f to its arguments only if there are arguments.
;	
;
;>(only +)
;#<procedure>
;
;>((only +) 1 2 3)
;6
;
;>((only +))
;nil
;
;/a> [code] [Macro] accum accfn [body ...]
;Executes body. Inside body, each time cfn is called, its argument is pushed on a list that becomes the return value. Note that the list is in reverse order.
;	
;
;>(accum accfn (each x '(1 2 3) (accfn (* x 10))))
;(30 20 10)
;
;[code] summing sumfn [body ...]
;Sums the number of times sumfn is called with a true argument in body. The sum is returned. The sumfn argument specifies the name under which the summing function is available to the body.
;	
;
;>(summing
;i
;true (map istrue '(1 nil nil t)))
;2
