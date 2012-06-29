(in-package :arc-compat.internal)

(def-suite arc-compat)

;[code] [Macro] mac name args [body ...]
(defmacro mac (name args &body body)
  "Creates a macro."
  `(defmacro ,name ,(if (consp args)
                        (arc-ll-to-cl-ll args)
                        `(&rest ,args))
     #|(arnesi:with-lisp1 ,@body)|#
     ,@body))

(defmacro def (name args &body body)
  `(defun ,name (,@(cl:if (consp args)
                          (arc-ll-to-cl-ll args)
                          `(&rest ,args)))
     ,@body))

(defmacro if (&rest args)
  (cond ((null args) ''nil)
        ((null (cdr args)) (car args))
        (T `(cl:let ((it ,(car args)))
              (cl:if (not (null it))
                     ,(cadr args)
                     (if ,@(cddr args)))))))


;(defmacro +INTERNAL-DEPARAM (param &body body)
;  (DESTRUCTURING-BIND ,param ,g
;    (DECLARE (IGNORABLE ,@(+internal-flatten args)))
;    ,@body))

(defmacro FN (args &body body)
  (cl:let ((g (gensym))
           (args (if (consp args)
                     args
                     `(&rest ,args))))
    `(LAMBDA (&rest ,g)
       (DESTRUCTURING-BIND ,args ,g
         (DECLARE (IGNORABLE ,@(+internal-flatten args)))
         ,@body))))

;; let var val [body ...]
(defmacro let (var val &body body)
  "The let statement sets the variable var to the value within the
scope of the body. Outside the let statement, any existing value
of var is unaffected. Let is like with but with a single variable
binding."
  `(DESTRUCTURING-BIND (,var) (list ,val)
     (DECLARE (IGNORABLE ,@(+internal-flatten `(,var))))
     ,@body))

;; with ([var val ...]) [body ...]
;>(with (a 1 b 2 c 3) (+ a b c))

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
                         (DECLARE (IGNORABLE ,@(+internal-flatten vars)))
                         ,@body))))

;; withs ([var val ...]) [body ...]

"Creates a new variable binding and executes the body. The values
are computed sequentially (like Scheme's let*, rather than
let). If the last variable doesn't have a value, it is assigned
nil."

(defmacro withs (binds &body body)
  (cl:let ((binds (cl:loop :for vv :on binds :by #'cddr
                        :collect `(,(car vv) ,(cadr vv)))))
    (cl:reduce (lambda (vv res) `(arc::let ,@vv ,res))
               binds
               :initial-value `(progn ,@body)
               :from-end 'T)))

;>(withs (a 1 b (+ a 1)) (+ a b))
;3

;(withs (a 1 b 2 (c d) '(3 4)) (+ a b c d))

(defmacro do (&body forms)
  `(progn ,@forms))
