(ns lisp-one-point-five.core-test
  (:require [clojure.test :refer :all]
            [lisp-one-point-five.core :refer :all]))

(deftest test-$cons
  (is (= '(X) ($cons 'X $NIL)))
  (is (= '(X A B C) ($cons 'X '(A B C))))
  (is (= '((A B C) X Y Z) ($cons '(A B C) '(X Y Z))))
  (is (= '(A B) ($cons 'A 'B))))

(deftest test-$atom
  (is (= $T ($atom 'X)))
  (is (= $T ($atom $NIL)))
  (is (= $F ($atom '(X))))
  (is (= $F ($atom '(X Y Z)))))

(deftest test-$eq
  (is (= $T ($eq 'ALPHA 'ALPHA)))
  (is (= $T ($eq $NIL $NIL)))
  (is (= $F ($eq 'ALPHA 'BETA)))
  (is (= $F ($eq $NIL 'ALPHA))))

(deftest test-$car
  (is (= 'A ($car '(A))))
  (is (= 'A ($car '(A B C D E)))))

(deftest test-$cdr
  (is (= $NIL ($cdr '(A))))
  (is (= '(B C D E) ($cdr '(A B C D E)))))

(deftest test-$cxxxr
  (is (= 'A   ($caar  '((A B) C D E))))
  (is (= 'C   ($cadr  '((A B) C D E))))
  (is (= '(B) ($cdar  '((A B) C D E))))
  (is (= 'D   ($caddr '((A B) C D E))))
  (is (= 'B   ($cadar '((A B) C D E)))))

(deftest test-$null
  (is (= $T ($null $NIL)))
  (is (= $F ($null '(A B C)))))

(deftest test-$pairlis
  (is (= $NIL
         ($pairlis $NIL $NIL $NIL)))
  (is (= '((A D) (B E) (C F))
         ($pairlis '(A B C) '(D E F) $NIL)))
  (is (= '((A B) (C D) (E F))
         ($pairlis $NIL $NIL '((A B) (C D) (E F)))))
  (is (= '((A D) (B E) (C F) (W X) (Y Z))
         ($pairlis '(A B C) '(D E F) '((W X) (Y Z))))))

(deftest test-$assoc
  (is (= '(A B) ($assoc 'A '((A B)))))
  (is (= '(E F) ($assoc 'E '((A B) (C D) (E F) (G H))))))