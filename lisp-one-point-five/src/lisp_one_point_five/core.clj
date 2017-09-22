(ns lisp-one-point-five.core)

(declare $apply $assoc $atom $car $caar $cadr $cadar
         $caddr $cdr $cdar $cons $eq $eval $evcon $evlis
         $null $pairlis)

(defmacro $cond
  ([[pred exp]]
   `(if ~pred ~exp))
  ([[pred exp] & clauses]
   `(if ~pred ~exp ($cond ~@clauses))))

(defn $evalquote
  [fun x]
  ($apply fun x ()))

(defn $apply
  [fun x a]
  ($cond

    (($atom fun)
     ($cond
       (($eq fun 'CAR)  ($caar x))
       (($eq fun 'CDR)  ($cdar x))
       (($eq fun 'CONS) ($cons ($car x) ($cadr x)))
       (($eq fun 'ATOM) ($atom ($car x)))
       (($eq fun 'EQ)   ($eq ($car x) ($cadr x)))
       (true            ($apply ($eval fun a) x a))))

    (($eq ($car fun) 'LAMBDA)
     ($eval ($caddr fun)
            ($pairlis ($cadr fun) x a)))

    (($eq ($car fun) 'LABEL)
     ($apply ($caddr fun)
             x
             ($cons ($cons ($cadr fun)
                           ($caddr fun))
                    a)))))

(defn $eval
  [e a]
  ($cond

    (($atom e)
     ($cadr ($assoc e a)))

    (($atom ($car e))
     ($cond
       (($eq ($car e) 'QUOTE) ($cadr e))
       (($eq ($car e) 'COND)  ($evcon ($cdr e) a))
       (true                  ($apply ($car e)
                                      ($evlis ($cdr e) a)
                                      a))))
    (true
     ($apply ($car e)
             ($evlis ($cdr e) a)
             a))))

(defn $evcon
  [c a]
  ($cond
    (($eval ($caar c) a) ($eval ($cadar c) a))
    (true                ($evcon ($cdr c) a))))

(defn $evlis
  [m a]
  ($cond
    (($null m) ())
    (true      ($cons ($eval ($car m) a)
                      ($evlis ($cdr m) a)))))

(defn $pairlis
  "This function gives the list of pairs of corresponding
  elements of the lists x and y, and appends this to the
  list a."
  [x y a]
  ($cond
    (($null x) a)
    (true      ($cons ($cons ($car x) ($car y))
                      ($pairlis ($cdr x) ($cdr y) a)))))

(defn $assoc
  "If a is an association list such as the one formed by
  $pairlis, then $assoc will produce the first pair
  whose first term is x. Thus it is a table searching
  function."
  [x a]
  ($cond
    (($eq ($caar a) x) ($car a)) ; Lisp 1.5 manual uses equal instead of eq.
    (true              ($assoc x ($cdr a)))))

(defn $cons
  "Obtains a new word from the free storage list and places
  its two arguments in the address and decrement of this word,
  respectively."
  [a b]
  ($cond
    ((seq? b) (cons a b))
    (true     (list a b))))

(defn $atom
  "Returns true if its argument is an atomic symbol, and
  false if its argument is composite. The empty list is
  considered atomic."
  [x]
  ($cond
    ((symbol? x)               true)
    ((and (seq? x) (empty? x)) true)
    (true                      false)))

(defn $eq
  "Returns true if both arguments are the same atomic
  symbol, otherwise returns false. It is undefined for
  non-atomic arguments."
  [a b]
  ($cond
    ((and ($atom a) ($atom b))
     (= a b))))

(def $car
  "Returns the first part of its composite argument. The
  car of an atomic symbol is undefined."
  first)

(def $cdr
  "Returns the second part of its composite argument. The
  cdr of an atomic symbol is undefined."
  rest)

(def $caar
  "Equivalent to: ($car ($car x))."
  (comp $car $car))

(def $cadr
  "Equivalent to: ($car ($cdr x))."
  (comp $car $cdr))

(def $cdar
  "Equivalent to: ($cdr ($car x))."
  (comp $cdr $car))

(def $caddr
  "Equivalent to: ($car ($cdr ($cdr x)))."
  (comp $car $cdr $cdr))

(def $cadar
  "Equivalent to: ($car ($cdr ($car x)))."
  (comp $car $cdr $car))

(def $null
  "Returns true if its argument is an empty list, or false
  otherwise."
  empty?)