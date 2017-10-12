(ns lisp-one-point-five.core)

(declare $apply $eval $evcon $evlis)

(def $T
  "Synonym for true."
  true)

(def $F
  "Synonym for false."
  false)

(def $NIL
  "Synonym for the empty (null) list."
  ())

(defmacro $cond
  "Conditional expression of the form:

     ($cond
       (p1 e1)
       (p2 e2)
       ...
       (pn en))

  where each pi is an expression whose value may be truth
  or falsity, and each ei is any expression. The meaning
  of a conditional expression is: if p1 is true, then the
  value of e1 is the value of the entire expression. If
  p1 is false, then if p2 is true the value of e2 is
  the value of the entire expression. The pi are searched
  from left to right until the first true one is found.
  Then the corresponding ei is selected. If none of the
  pi are true, then the value of the entire expression
  is undefined."
  ([[pred exp]]
   `(if ~pred ~exp))
  ([[pred exp] & clauses]
   `(if ~pred ~exp ($cond ~@clauses))))

(defn $cons
  "Obtains a new word from the free storage list and places
  its two arguments in the address and decrement of this word,
  respectively."
  [a b]
  ($cond
    ((seq? b) (cons a b))
    ($T       (list a b))))

(defn $atom
  "Returns true if its argument is an atomic symbol, and
  false if its argument is composite. The empty list is
  considered atomic."
  [x]
  ($cond
    ((symbol? x)               $T)
    ((and (seq? x) (empty? x)) $T)
    ($T                        $F)))

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
  "Returns the second part (rest) of its composite
  argument. The cdr of an atomic symbol is undefined."
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

(defn $pairlis
  "This function gives the list of pairs of corresponding
  elements of the lists x and y, and appends this to the
  list a."
  [x y a]
  ($cond
    (($null x) a)
    ($T        ($cons
                 ; Lisp 1.5 divergence: original creates dotted pair.
                 ($cons ($car x) ($cons ($car y) $NIL))
                 ($pairlis ($cdr x) ($cdr y) a)))))

(defn $assoc
  "If a is an association list such as the one formed by
  $pairlis, then $assoc will produce the first pair
  whose first term is x. Thus it is a table searching
  function."
  [x a]
  ($cond
    ; Lisp 1.5 divergence: original uses equal instead of eq.
    (($eq ($caar a) x) ($car a))
    ($T                ($assoc x ($cdr a)))))

(defn $evalquote
  "A universal Lisp function. When evalquote is given a
  function fun and a list of arguments x for that
  function, it computes the value of the function applied
  to the arguments."
  [fun x]
  ($apply fun x $NIL))

(defn $apply
  "Handles a function fun and its arguments in the list x.
  The argument a is used as an association list for
  storing the values of bound variables and function
  names."
  [fun x a]
  ($cond

    (($atom fun)
     ($cond
       (($eq fun 'CAR)  ($caar x))
       (($eq fun 'CDR)  ($cdar x))
       (($eq fun 'CONS) ($cons ($car x) ($cadr x)))
       (($eq fun 'ATOM) ($atom ($car x)))
       (($eq fun 'EQ)   ($eq ($car x) ($cadr x)))
       ($T              ($apply ($eval fun a) x a))))

    (($eq ($car fun) 'LAMBDA)
     ($eval ($caddr fun) ($pairlis ($cadr fun) x a)))

    (($eq ($car fun) 'LABEL)
     ($apply
       ($caddr fun)
       x
       ($cons ($cons
                 ($cadr fun)
                 ; Lisp 1.5 divergence: original creates dotted pair.
                 ($cons ($caddr fun) $NIL))
              a)))))

(defn $eval
  "Handles the forms in e. The argument a is used as an
  association list for storing the values of bound
  variables and function names."
  [e a]
  ($cond

    (($atom e)
     ; Lisp 1.5 divergence: original uses cdr on dotted pair.
     ($cadr ($assoc e a)))

    (($atom ($car e))
     ($cond
       (($eq ($car e) 'QUOTE) ($cadr e))
       (($eq ($car e) 'COND)  ($evcon ($cdr e) a))
       ($T                    ($apply ($car e)
                                      ($evlis ($cdr e) a)
                                      a))))
    ($T
     ($apply ($car e)
             ($evlis ($cdr e) a)
             a))))

(defn $evcon
  "Evaluates the propositional terms in order, and chooses
  the form following the first true predicate. The argument
  a is used as an association list for storing the values
  of bound variables and function names."
  [c a]
  ($cond
    (($eval ($caar c) a) ($eval ($cadar c) a))
    ($T                  ($evcon ($cdr c) a))))

(defn $evlis
  "Evaluate all expressions in list m. The argument a is
  used as an association list for storing the values of
  bound  variables and function names."
  [m a]
  ($cond
    (($null m) $NIL)
    ($T        ($cons ($eval ($car m) a)
                      ($evlis ($cdr m) a)))))
