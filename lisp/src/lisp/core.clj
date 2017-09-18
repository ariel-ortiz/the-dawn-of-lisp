(ns lisp.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(declare $apply $atom? $eq? $car $cdr $caar $cadr $cdar
         $eval $caddr $pairlis $cons $assoc $evcon $evlis
         $cadar $null?)

(defn $evalquote
  [fun x]
  ($apply fun x ()))

(defn $apply
  [fun x a]
  (cond

    ($atom? fun)
    (cond
      ($eq? fun 'CAR)  ($caar x)
      ($eq? fun 'CDR)  ($cdar x)
      ($eq? fun 'CONS) ($cons ($car x) ($cadr x))
      ($eq? fun 'ATOM) ($atom? ($car x))
      ($eq? fun 'EQ)   ($eq? ($car x) ($cadr x))
      true             ($apply ($eval fun a) x a))

    ($eq? ($car fun) 'LAMBDA)
    ($eval ($caddr fun)
           ($pairlis ($cadr fun) x a))

    ($eq? ($car fun) 'LABEL)
    ($apply ($caddr fun)
            x
            ($cons ($cons ($cadr fun)
                          ($caddr fun))
                   a))))

(defn $eval
  [e a]
  (cond

    ($atom? e)
    ($cadr ($assoc e a))

    ($atom? ($car e))
    (cond
      ($eq? ($car e) 'QUOTE) ($cadr e)
      ($eq? ($car e) 'COND)  ($evcon ($cdr e) a)
      true                   ($apply ($car e)
                                     ($evlis ($cdr e) a)
                                     a))
    true
    ($apply ($car e)
            ($evlis ($cdr e) a)
            a)))

(defn $evcon
  [c a]
  (cond
    ($eval ($caar c) a) ($eval ($cadar c) a)
    true                ($evcon ($cdr c) a)))

(defn $evlis
  [m a]
  (cond
    ($null? m) ()
    true       ($cons ($eval ($car m) a)
                      ($evlis ($cdr m) a))))

(defn $pairlis
  "This function gives the list of pairs of corresponding
  elements of the lists x and y, and appends this to the
  list a."
  [x y a]
  (cond
    ($null? x) a
    true       ($cons ($cons ($car x) ($car y))
                      ($pairlis ($cdr x) ($cdr y) a))))

(defn $assoc
  "If a is an association list such as the one formed by
  $pairlis, then $assoc will produce the first pair
  whose first term is x. Thus it is a table searching
  function."
  [x a]
  (cond
    ($eq? ($caar a) x) ($car a) ; Lisp 1.5 manual uses equal
    true               ($assoc x ($cdr a))))

(defn $cons
  [a b]
  (if (seq? b)
    (cons a b)
    (list a b)))

(defn $atom?
  [x]
  (or (symbol? x) (empty? x)))

(def $eq? =)

(def $car first)

(def $cdr rest)

(def $caar (comp $car $car))

(def $cadr (comp $car $cdr))

(def $cdar (comp $cdr $car))

(def $caddr (comp $car $cdr $cdr))

(def $cadar (comp $car $cdr $car))

(def $null? empty?)
