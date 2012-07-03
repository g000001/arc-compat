(in-package :arc-compat.internal)
(in-suite arc-compat)


(def newstring (length (o char #\Nul))
  "Creates a string of the given length."
  (cl:make-string length :initial-element char))


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


(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))


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


