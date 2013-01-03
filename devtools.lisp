(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)


(def arc-compat-external-symbols ()
  (cl:loop :for s :being :each :external-symbol :in :arc :collect s))


(def unimprements ()
  (let arcsyms (arc-compat-external-symbols)
    (let unimps (keep (fn (_) (and (not (cl:fboundp _))
                                 (not (cl:boundp _))))
                      arcsyms)
      (cl:values unimps (len arcsyms) (len unimps)))))


(def *arc-version ()
  (*let (_ all unimp) (unimprements)
    (- 1000 unimp)))

