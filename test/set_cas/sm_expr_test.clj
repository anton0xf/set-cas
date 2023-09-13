(ns set-cas.sm-expr-test
  (:require [set-cas.sm-expr :refer :all]
            [clojure.test :refer :all]))

(deftest to-m-expr-test
  (is (= "A" (to-m-expr 'A)))
  (is (= "A + B" (to-m-expr '(+ A B))))
  (is (= "A + B + C" (to-m-expr '(+ A B C))))
  (is (= "A + (B - C)" (to-m-expr '(+ A (- B C)))))
  (is (= "A = B - C" (to-m-expr '(= A (- B C))))))
