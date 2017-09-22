# Lisp 1.5

This source code was developed for the presentation “The Dawn of Lisp, or: How to Write Eval and Apply in Clojure” for [Clojure/conj 2017](http://2017.clojure-conj.org/).

## Usage

    (use 'lisp-one-point-five.core)
    
    ($evalquote 'CAR '((A B C)))
    => A
    
    ($evalquote 'CONS '(A (X Y Z)))
    => (A X Y Z)

## License

Copyright © 2017 by Ariel Ortiz.

Distributed under the GNU General Public License v3.0.
