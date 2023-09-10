(ns set-cas.core-test
  (:require [set-cas.core :refer :all]
            [clojure.test :refer :all]))

(deftest test-to-bin
  (is (= '(+) (to-bin '(+))))
  (is (= '(+ A) (to-bin '(+ A))))
  (is (= '(+ A B) (to-bin '(+ A B))))
  (is (= '(+ A (+ B C)) (to-bin '(+ A B C))))
  (is (= '(+ (* A (* B C)) D) (to-bin '(+ (* A B C) D))))
  (is (= '(+ A (+ B (+ C D))) (to-bin '(+ A B C D))))
  (is (= '(+ A (+ B (* C (* D E)))) (to-bin '(+ A B (* C D E)))))
  (is (= '(- (- A B) C) (to-bin '(- A B C))))
  (is (= '(- (- (- A B) C) D) (to-bin '(- A B C D))))
  (is (= '(= A (+ B (+ C D)) E) (to-bin '(= A (+ B C D) E)))))

