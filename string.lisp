(in-package :arc-compat.internal)
(in-readtable :common-lisp)
(in-suite arc-compat)

;;;                       String operations
;;;============================================================================
;;; This software is copyright (c) Paul Graham and Robert Morris.  Permission
;;; to use it is granted under the Perl Foundations's Artistic License 2.0.
;;; 
;;; Ported to Common Lisp by CHIBA Masaomi.


(def whitec (c)
  "Predicate to test if a character is whitespace 
  (space, newline, tab, or return)."
  (in c #\space #\newline #\tab #\return))


(tst whitec
  (== (whitec #\tab)
      t)
  (== (whitec "  ")
      nil))


(def nonwhite (c)
  "Predicate to test if a character is not whitespace (space, newline, tab, 
  or return)."
  (no (whitec c)))


(tst nonwhite
  (== (nonwhite #\tab)
      nil)
  (== (nonwhite #\a)
      t))


(def letter (c) (cl:or (<= #\a c #\z) (<= #\A c #\Z)))


(def digit (c) (<= #\0 c #\9))


#|(def alphadig (c)
  "Predicate to test if a character is alphabetic or a digit."
  (or (letter c) (digit c)))|#


(defalias alphadig cl:alphanumericp
  "Predicate to test if a character is alphabetic or a digit.")


(tst alphabetic
  (== (alphadig #\A)
      t )
  (== (alphadig #\2)
      t ))


(def punc (c)
  "Predicate to detemine if c is a punctuation character in the set: .,;:!?"
  (in c #\. #\, #\; #\: #\! #\?))


(tst punc
  (== (punc #\.)
      t )
  (== (punc #\a)
      nil )
  (== (punc ".")
      nil ))


(def downcase (x)
  "Converts a string, character, or symbol to lower case. This only converts
  ASCII; Unicode is unchanged."
  (case (type x)
    string (map #'cl:char-downcase x)
    char   (cl:char-downcase x)
    sym    (sym (map #'char-downcase (coerce x 'string)))
           (err "Can't downcase" x)))


(tst downcase
  (== (downcase "abcDEF123")
      "abcdef123" )
  (== (downcase #\A)
      #\a )
  (== (downcase '|abcDEF123|)
      '|abcdef123| ))


(def upcase (x)
  "Converts a string, character, or symbol to lower case. This only converts
  ASCII; Unicode is unchanged."
  (case (type x)
      string (map #'cl:char-upcase x)
      char   (cl:char-upcase x)
      sym    (sym (map #'cl:char-upcase (coerce x 'string)))
             (err "Can't upcase" x)))


(tst upcase
  (== (upcase "abcDEF123")
      "ABCDEF123" )
  (== (upcase #\a)
      #\A )
  (== (upcase '|abcDEF123|)
      'ABCDEF123 ))


(def ellipsize (str (o limit 80))
  "If str is longer than the limit (default 80), truncate it and append 
  ellipses ('...')."
  (cl:if (<= (len str) limit)
         str
         (+ (cut str 0 limit) "...")))


(tst ellipsize
  (== (ellipsize "Too long" 6)
      "Too lo..."))


(def rand-string (n)
  "Generates a random string of alphanumerics of length n."
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0 salt (get-internal-real-time))
      (while (< i n)
        (let x (mod (+ salt (cl:random 256)) 256)
          (unless (> x 247)
            (setf (elt s i) (elt c (mod x nc)))
            (++ i))))
      s)))


(tst rand-string 
  (== T (let str (rand-string 6) 
          (cl:and (is 6 (len str)) 
                  (all #'alphadig str)))))


(def string args
  "Converts the args into a string. The args must be coerce-able to a string."
  (apply #'+
         (make-string 0)
         (map (fn (_) (coerce _ 'string)) 
              (rem nil args))))


(tst string
  (== (string 2 'a '(#\b #\c))
      "2Abc")
  (== (string 2 'a #\a #\b)
      "2Aab"))


(def recstring (test s (o start 0))
  "Recursively steps through the string until f returns a non-nil value, and 
  returns that value. Returns nil otherwise. The values passed to f are integer 
  indices; the indices start at 0, or start if specified."
  (let n (len s)
    n                                   ;ignore
    (funcall
     (afn (i)
       (cl:and (< i (len s))
               (cl:or (funcall test i)
                      (self (+ i 1)))))
     start)))


(tst recstring
  (== (let str "abcde"
           (recstring
            (fn (idx) (cl:if (is (cl:char str idx) #\c) (+ 10 idx)))
            str ))
      12 ))


(cl:defvar .bar*. " | ")
(define-symbol-macro bar* .bar*.)


(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (cl:if ,needbars
                                (pr bar* ,out)
                                (do (set ,needbars t)
                                    (pr ,out))))))
                  body)))))


(tst w/bars
  (== (with-output-to-string (*standard-output*)
        (w/bars (pr "a") 42 (pr "b") (pr "c") nil))
      "a | b | c")
  (let bar* " - "
    (== (with-output-to-string (*standard-output*)
          (w/bars (pr "a") 42 (pr "b") (pr "c") nil))
        "a - b - c")))


(def headmatch (pat seq (o start 0))
  (let p (len pat) 
    (funcall (afn (i)      
       (cl:or (is i p) 
              (cl:and (is (ref pat i) (ref seq (+ i start)))
                      (self (+ i 1)))))
     0)))


(def subst (new old seq)
  (let boundary (+ (- (len seq) (len old)) 1)
    (tostring 
      (forlen i seq
        (cl:if (cl:and (< i boundary) (headmatch old seq i))
               (do (++ i (- (len old) 1))
                   (pr new))
               (pr (ref seq i)))))))


;;;                      strings.arc library
;;;============================================================================


;;; eof

