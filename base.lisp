(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(def-suite arc-compat)


(defmacro mac (name args &body body)
  "Creates a macro."
  `(defmacro ,name ,(cl:if (listp args)
                           (arc-ll-to-cl-ll args)
                           `(&rest ,args))
     #|(arnesi:with-lisp1 ,@body)|#
     ,@body))


(defmacro def (name args &body body)
  `(defun ,name (,@(cl:if (listp args)
                          (arc-ll-to-cl-ll args)
                          `(&rest ,args)))
     ,@body))


(defmacro if (&rest args)
  (cond ((null args) ''nil)
        ((null (cdr args)) (car args))
        (T `(cl:if ,(car args)
                   ,(cadr args)
                   (if ,@(cddr args))))))


(defalias or cl:or)


(defalias and cl:and)


(defalias atom cl:atom)


(defalias complement cl:complement)


(defmacro when (pred &body body)
  `(if ,pred (cl:progn ,@body)))


(defmacro aif (&rest args)
  (cond ((null args) ''nil)
        ((null (cdr args)) (car args))
        (T `(cl:let ((it ,(car args)))
              (cl:if (not (null it))
                     ,(cadr args)
                     (aif ,@(cddr args)))))))


;(defmacro +INTERNAL-DEPARAM (param &body body)
;  (DESTRUCTURING-BIND ,param ,g
;    (DECLARE (IGNORABLE ,@(+internal-flatten args)))
;    ,@body))

(defmacro FN (args &body body)
  (cl:let ((g (gensym))
           (args (cl:if (consp args)
                        args
                        `(&rest ,args))))
    `(LAMBDA (&rest ,g)
       (DESTRUCTURING-BIND ,args ,g
         (cl:DECLARE 
          (cl:IGNORABLE ,@(remove-if (lambda (x)
                                       (member x cl:lambda-list-keywords))
                                     (+internal-flatten args))))
         ,@body))))


(defmacro let (var val &body body)
  "The let statement sets the variable var to the value within the
  scope of the body. Outside the let statement, any existing value
  of var is unaffected. Let is like with but with a single variable
  binding."
  (if (consp var)
      (cl:let ((tem (gensym "let-")))
        `((CL:LAMBDA (&REST ,tem)
            (cl:DECLARE (cl:DYNAMIC-EXTENT ,tem))
            (DESTRUCTURING-BIND (,var) ,tem
              (cl:DECLARE (cl:IGNORABLE ,@(+internal-flatten `(,var))))
              ,@body ))
          ,val))
      `(CL:LET ((,var ,val)) 
         ,@body)))


(defmacro leto (var val &body body)
  `(cl:let ((,var ,val))
     (w/obcall (,var)
       ,@body)))


(defmacro with (binds &body body)
  "Creates a new variable binding and executes the body. The values
  are computed before any of the assignments are done (like
  Scheme's let, rather than let*). If the last variable doesn't
  have a value, it is assigned nil."
  (cl:loop :for x :on binds :by #'cddr
           :collect (first x) :into vars
           :collect (second x) :into vals
           :finally (return
                      `(DESTRUCTURING-BIND ,vars (list ,@vals)
                         (cl:DECLARE (cl:IGNORABLE ,@(+internal-flatten vars)))
                         ,@body))))


(defmacro witho (binds &body body)
  (cl:loop :for x :on binds :by #'cddr
           :collect (first x) :into vars
           :collect (second x) :into vals
           :finally (return
                      `(DESTRUCTURING-BIND ,vars (list ,@vals)
                         (cl:DECLARE (cl:IGNORABLE ,@(+internal-flatten vars)))
                         (w/obcall (,vars)
                           ,@body)))))


(defmacro withs (binds &body body)
  "Creates a new variable binding and executes the body. The values
  are computed sequentially (like Scheme's let*, rather than
  let). If the last variable doesn't have a value, it is assigned
  nil."
  (cl:let ((binds (cl:loop :for vv :on binds :by #'cddr
                        :collect `(,(car vv) ,(cadr vv)))))
    (cl:reduce (lambda (vv res) `(arc::let ,@vv ,res))
               binds
               :initial-value `(progn ,@body)
               :from-end 'T)))


(defmacro withos (binds &body body)
  (cl:let ((binds (cl:loop :for vv :on binds :by #'cddr
                     :collect `(,(car vv) ,(cadr vv)))))
          (cl:reduce (lambda (vv res) `(arc::let ,@vv ,res))
                     binds
                     :initial-value `(w/obcall (,@(mapcar #'car binds))
                                       ,@body)
                     :from-end 'T)))


;>(withs (a 1 b (+ a 1)) (+ a b))
;3

;(withs (a 1 b 2 (c d) '(3 4)) (+ a b c d))

(defmacro do (&body forms)
  `(progn ,@forms))


(defun map (fn seq &rest more-seqs)
  "Applies f to the elements of the sequences, taking the first from each, 
  the second from each, and so on. If there are n sequences, f must be a function 
  accepting n arguments. The sequences can be lists or strings. If any sequence is
  a string, then f must return a character, and the result will be a string made
  up of the results from f. Otherwise, the result will be a list of the results 
  from f. The sequences are processed up to the length of the shortest sequence. 
  For a single list, map is the same as map1."
  (cl:apply #'cl:map (cl:type-of seq)
            fn
            seq
            more-seqs))


(tst map
  (== (map (fn (a b c) (+ (* a 100) (* b 10) c))
        '(1 2 3) '(4 5 6) '(7 8 9 10))
      '(147 258 369))
  (== (map (fn (_) (list _ (* _ 10))) '(1 2 3))
      '((1 10) (2 20) (3 30)))
  (== (map cdr '((1) (2 3) (4 5)))
    '(nil (3) (5)))
  (== (map (fn (c n) (coerce (+ n (coerce c 'int)) 'char)) "abc" '(0 2 4))
    "adg")
  (== (map min "bird" "elephant")
    "bied"))


(defalias rand cl:random)


#|(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))|#


(defmacro compose (&rest args)
  (let g (uniq "compose-arg-")
    `(cl:lambda (&rest ,g)
       (cl:declare (cl:dynamic-extent ,g))
       ,(funcall
         (labels ((self (fs)
                    (cl:if (cdr fs)
                           (list 'cl:funcall (car fs) (self (cdr fs)))
                           `(apply ,(if (car fs) (car fs) 'idfn) ,g))))
           #'self)
         args))))


(defalias do1 cl:prog1
  "Saves the first expression and returns it after executing the body.")


(tst do1
  (== (let x 42 (do1 x (= x 50)))
      42))


(defun coerce (thing type)
  (typecase thing
    (char (cl:case type
            (string (cl:string thing))
            (cl:otherwise (cl:coerce thing type))))
    (sym (coerce (cl:string thing) type))
    (int (cl:case type
           (string (write-to-string thing))
           (cl:otherwise (cl:coerce thing type))))
    (t (cl:case type
         (sym (intern (cl:string thing)))
         (cl:otherwise (cl:coerce thing type))))))


(defun sym (x) (coerce x 'sym))


(defun len (seq)
  "Computes the length of seq."
  (etypecase seq
    (cl:sequence (cl:length seq))
    (table (cl:hash-table-count seq))))


(tst len
  (== (len "abc")
      3)
  (== (len '(1 2 3))
      3)
  (== (len (obj a 1 b 2))
      2))


;;; eof


