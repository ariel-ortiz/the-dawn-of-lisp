(ns lisp.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(declare $apply $atom? $eq? $car $cdr $caar $cadr $cdar
         $eval $caddr $pairlis $cons $assoc $evcon $evlis)

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




(defn $cons
  [a b]
  (if (list? b)
    (cons a b)
    (cons a (cons b ()))))

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
