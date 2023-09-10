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

(deftest test-replace-vars
  (is (= 'A (replace-vars 'X {'X 'A})))
  (is (= 'Y (replace-vars 'Y {'X 'A})))
  (is (= '(+ A Y) (replace-vars '(+ X Y) {'X 'A})))
  (is (= '(+ A B) (replace-vars '(+ X Y) {'X 'A, 'Y 'B})))
  (is (= '(+ A (- A B)) (replace-vars '(+ A (- X B)) {'X 'A}))))

(deftest test-subst
  (is (= '(+ B A) (subst '(+ A B) union-comm {'X 'A, 'Y 'B})))
  (is (= '(* (+ B A) C) (subst '(* (+ A B) C) union-comm {'X 'A, 'Y 'B})))
  (is (= '(+ (+ B C) A) (subst '(+ A (+ B C)) union-comm
                               {'X 'A, 'Y '(+ B C)})))
  (is (= '(+ A (+ C B)) (subst '(+ A (+ B C)) union-comm {'X 'B, 'Y 'C}))))
