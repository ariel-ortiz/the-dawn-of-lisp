(ns lisp-one-point-five.core-test
  (:require [clojure.test :refer :all]
            [lisp-one-point-five.core :refer :all]))

(deftest test-$cons
  (is (= '(X) ($cons 'X ())))
  (is (= '(X A B C) ($cons 'X '(A B C))))
  (is (= '((A B C) X Y Z) ($cons '(A B C) '(X Y Z))))
  (is (= '(A B) ($cons 'A 'B))))

(deftest test-$atom
  (is ($atom 'X))
  (is ($atom ()))
  (is (not ($atom '(X))))
  (is (not ($atom '(X Y Z)))))

(deftest test-$eq
  (is ($eq 'ALPHA 'ALPHA))
  (is ($eq () ()))
  (is (not ($eq 'ALPHA 'BETA)))
  (is (not ($eq () 'ALPHA))))

(deftest test-$car
  (is (= 'A ($car '(A))))
  (is (= 'A ($car '(A B C D E)))))

(deftest test-$cdr
  (is (= () ($cdr '(A))))
  (is (= '(B C D E) ($cdr '(A B C D E)))))

(deftest test-$cxxxr
  (is (= 'A   ($caar  '((A B) C D E))))
  (is (= 'C   ($cadr  '((A B) C D E))))
  (is (= '(B) ($cdar  '((A B) C D E))))
  (is (= 'D   ($caddr '((A B) C D E))))
  (is (= 'B   ($cadar '((A B) C D E)))))

(deftest test-$null
  (is ($null ()))
  (is (not ($null '(A B C)))))
